module Frontend exposing (Model, app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Common exposing (notifyIn)
import Element exposing (Attribute, Element, clipX, el, fill, height, maximum, paddingEach, paragraph, scrollbarX, scrollbarY, shrink, text, width)
import Element.Background
import Element.Font as Font
import Element.Input
import Element.Lazy
import Elm
import Elm.Parser
import Elm.Parser.Comments
import Elm.Parser.Declarations
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.ToString
import Eval.Expression
import Eval.Module
import FastDict as Dict exposing (Dict)
import Hash
import Html
import Html.Attributes
import Http
import IdDict
import Interactives
import InterpreterTypes exposing (Env, Error(..), Eval, PartiallyAppliedFunction(..), Value(..))
import Json.Decode
import Kernel
import Kernel.Html
import Lamdera exposing (sendToBackend)
import List.Extra exposing (Step(..))
import Markdown.Parser
import Markdown.Renderer
import Markdown.Renderer.ElmUi
import Maybe.Extra
import Parser exposing (DeadEnd)
import ParserFast
import Regex
import Result.Extra
import Task
import ToString exposing (annotationToString, errorToString, evalErrorDataToString, httpErrorToString, patternToString)
import Types exposing (BackendMsg(..), Cell(..), Code(..), FileName(..), FrontendModel, FrontendMsg(..), FullCode(..), Function, FunctionName(..), HostViewer, IdDict, Interactives(..), Markdown(..), Output, OutputError(..), OutputValue(..), ParameterName(..), ParsedSection, RawInteractiveValue(..), Section(..), ToBackend(..), ToFrontend(..), TypeName(..), Viewer)
import UI.Source as Source
import Url
import Value
import Viewers



-- MAIN


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , currentFileName = Nothing
      , source = Nothing
      , checksum = Nothing
      , error = ""
      , fileList = []
      , sections = []
      , interactives = Interactives.empty
      , functions = IdDict.empty (\(FunctionName functionName) -> functionName)
      , viewers = []
      , hostViewers = []
      , outputs = IdDict.empty (\(FunctionName functionName) -> functionName)
      , reloadRequests = IdDict.empty (\(FunctionName functionName) -> functionName)
      }
    , Cmd.batch
        [ Http.get
            { url = "/_x/list/pages/"
            , expect = Http.expectJson GotList (Json.Decode.list Json.Decode.string)
            }
        , sendToBackend RequestStartup
        , notifyIn Poll 4000
        ]
    )


updateFullSource : FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFullSource model =
    case ( model.source, model.checksum ) of
        ( Just (FullCode source), Just modelChecksum ) ->
            let
                checksum =
                    source |> Hash.fromString |> Hash.toString
            in
            if checksum == modelChecksum then
                ( model, Cmd.none )

            else
                ( { model | checksum = Just checksum }
                , Cmd.batch
                    [ sendToBackend (NewChecksumToBackend checksum)
                    ]
                )

        _ ->
            ( model, Cmd.none )


generateInteractive : FullCode -> Model -> Model
generateInteractive source model =
    let
        maybeFile : Result Error File
        maybeFile =
            makeFile source

        maybeEnv : Result Error Env
        maybeEnv =
            maybeFile
                |> Result.andThen Eval.Module.buildInitialEnv

        ( viewers, sections, functions ) =
            evaluateSections source maybeEnv
    in
    { model
        | source = Just source
        , functions = functions
        , viewers = viewers
        , hostViewers = makeHostViewers maybeEnv
        , sections = sections
    }


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotText result ->
            case result of
                Ok source ->
                    ( generateInteractive (FullCode source) model, notifyIn CheckGenerateOutputs 100 )

                Err error ->
                    ( { model
                        | error =
                            httpErrorToString error
                      }
                    , Cmd.none
                    )

        GotList result ->
            case result of
                Ok list ->
                    ( { model
                        | fileList =
                            list
                                |> List.filter (String.endsWith ".elm")
                                |> List.filter (\x -> not (String.endsWith "Host.elm" x))
                                |> List.map FileName
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | error =
                            httpErrorToString error
                      }
                    , Cmd.none
                    )

        WroteText _ ->
            ( model, Cmd.none )

        ListItemClicked fileName ->
            ( { model
                | currentFileName = Just fileName
                , outputs = IdDict.clear model.outputs
              }
            , sendToBackend (NewFilenameToBackend fileName)
            )

        InteractiveUpdated names value ->
            let
                newInteractives : Interactives
                newInteractives =
                    Interactives.insert names value model.interactives

                functionName =
                    Tuple.first names

                requestIndex : Int
                requestIndex =
                    IdDict.get functionName model.reloadRequests
                        |> Maybe.withDefault 0
                        |> (\x -> x + 1)
            in
            ( { model
                | interactives = newInteractives
                , reloadRequests = IdDict.insert functionName requestIndex model.reloadRequests
              }
            , Cmd.batch
                [ sendToBackend (InteractivesToBackend newInteractives)
                , notifyIn (ReloadFunction functionName requestIndex) 400
                ]
            )

        CheckGenerateOutputs ->
            case ( IdDict.isEmpty model.functions, Maybe.Extra.isJust model.source, IdDict.isEmpty model.outputs ) of
                ( False, True, True ) ->
                    let
                        newOutputs =
                            IdDict.map
                                (\_ { function, declaration } ->
                                    applyPartiallyApplied model.interactives function declaration
                                )
                                model.functions
                    in
                    ( { model | outputs = newOutputs }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReloadFunction functionName requestIndex ->
            if requestIndex == (IdDict.get functionName model.reloadRequests |> Maybe.withDefault 0) then
                let
                    newOutput : Result OutputError OutputValue
                    newOutput =
                        calculateOutput model.interactives model.functions functionName
                in
                ( { model | outputs = IdDict.insert functionName newOutput model.outputs }, Cmd.none )

            else
                ( model, Cmd.none )

        Poll ->
            let
                checkViewport : Cmd FrontendMsg
                checkViewport =
                    Task.perform (\viewport -> NewScroll viewport.viewport.y) Browser.Dom.getViewport
            in
            ( model, Cmd.batch [ checkViewport, notifyIn Poll 4000 ] )

        NewScroll y ->
            ( model, sendToBackend (NewScrollToBackend y) )

        NoOp ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        Startup { interactives, scroll, checksum, fileName } ->
            let
                ( newModel, cmd ) =
                    updateFullSource
                        { model
                            | interactives = interactives
                            , checksum = Just checksum
                            , currentFileName = fileName
                        }
            in
            ( newModel
            , Cmd.batch
                [ Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 scroll)
                , cmd
                , case fileName of
                    Just currentFileName ->
                        requestPage currentFileName

                    Nothing ->
                        notifyIn CheckGenerateOutputs 100
                ]
            )

        RequestNewSourceToFrontend ->
            case model.source of
                Just source ->
                    ( model
                    , Cmd.none
                      --, Http.post
                      --    { url = "/_x/write/src/Host.elm"
                      --    , body = Http.stringBody "application/text" (getHostText source)
                      --    , expect = Http.expectWhatever WroteText
                      --    }
                    )

                Nothing ->
                    ( { model | error = "Requested new source when there was no source yet" }, Cmd.none )

        RequestNewFileNameToFrontend fileName ->
            ( model
            , requestPage fileName
            )


requestPage : FileName -> Cmd FrontendMsg
requestPage (FileName fileName) =
    Http.get
        { url = "/_x/read/pages/" ++ fileName
        , expect = Http.expectString GotText
        }



-- PARSING TEXT INPUT


parseSections : FullCode -> List ( Code, ParsedSection )
parseSections (FullCode fullSource) =
    let
        newSectionRegex : Regex.Regex
        newSectionRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "\n\n+(?=[^\\s])"
    in
    fullSource
        |> String.replace "\u{000D}\n" "\n"
        |> Regex.split newSectionRegex
        |> List.map (\source -> ( Code source, parseSection (Code source) ))


parseSection : Code -> Result (List DeadEnd) (List Cell)
parseSection (Code source) =
    let
        parser : ParserFast.Parser (List Cell)
        parser =
            ParserFast.skipWhileWhitespaceFollowedBy
                (ParserFast.loopWhileSucceeds
                    (ParserFast.oneOf2
                        (ParserFast.map (\x -> CellDeclaration x.syntax) Elm.Parser.Declarations.declaration)
                        (ParserFast.map (\x -> x |> Node.map Markdown |> CellComment) Elm.Parser.Comments.singleLineComment)
                        |> ParserFast.followedBySkipWhileWhitespace
                    )
                    []
                    (\cell list -> cell :: list)
                    (\list -> List.reverse list)
                )
    in
    ParserFast.run parser source


getHostText : FullCode -> String
getHostText fullSource =
    let
        maybeFile : Result Error File
        maybeFile =
            makeFile fullSource

        maybeDeclarations : Maybe (List String)
        maybeDeclarations =
            Result.toMaybe maybeFile
                |> Maybe.map
                    (\file ->
                        file
                            |> .declarations
                            |> List.map Node.value
                            |> List.map extractNameFromDeclaration
                    )

        maybeDeclarationList : List String
        maybeDeclarationList =
            maybeDeclarations
                |> Maybe.withDefault []
                |> List.map (\name -> Elm.tuple (Elm.string name) (Elm.val name))
                |> Elm.list
                |> Elm.declaration "functionList"
                |> Elm.ToString.declaration
                |> List.map .body

        (FullCode fullSourceText) =
            fullSource
    in
    fullSourceText
        ++ "\n\n"
        ++ String.join "\n" maybeDeclarationList



-- SECTIONS EVALUATION


evaluateSections : FullCode -> Result Error Env -> ( List Viewer, List Section, IdDict FunctionName Types.Function )
evaluateSections source maybeEnv =
    let
        viewerSelector : Kernel.InSelector (List (Value -> InterpreterTypes.Config -> Env -> InterpreterTypes.EvalResult (Maybe Kernel.Html.Html))) {}
        viewerSelector =
            Kernel.listIn (Kernel.function Eval.Expression.evalFunction Kernel.anything Kernel.to (Kernel.maybe Kernel.html))

        viewersValue : Result OutputError Value
        viewersValue =
            evaluateName maybeEnv (FunctionName "viewers")

        transformValue : (Value -> InterpreterTypes.Config -> Env -> InterpreterTypes.EvalResult (Maybe Kernel.Html.Html)) -> Viewer
        transformValue func =
            case maybeEnv of
                Err error ->
                    \_ ->
                        error
                            |> errorToString
                            |> OutputError
                            |> Err

                Ok env ->
                    \value ->
                        func value { trace = False } env
                            |> (\( a, _, _ ) -> a)
                            |> Result.mapError evalErrorDataToString
                            |> Result.mapError OutputError
                            |> Result.map (\maybe -> Maybe.map Kernel.Html.htmlToReal maybe)

        viewers : List Viewer
        viewers =
            viewersValue
                |> Result.toMaybe
                |> Maybe.andThen viewerSelector.fromValue
                |> Maybe.withDefault []
                |> List.map transformValue

        evaluateSection : ( Code, ParsedSection ) -> ( Section, Maybe ( FunctionName, Function ) )
        evaluateSection sectionResult =
            sectionFromParsed maybeEnv sectionResult

        viewersError : Maybe OutputError
        viewersError =
            case viewersValue of
                Ok _ ->
                    Nothing

                Err (OutputError error) ->
                    -- This exact error means that the user has not supplied a "viewers" at all
                    -- That's okay, they don't need to! We just want to let them know why their
                    -- viewers are funky if they do expect them to show up
                    if String.startsWith "NameError: " error && String.endsWith "viewers" error then
                        Nothing

                    else
                        Just (OutputError error)

        parsedSections : List ( Code, ParsedSection )
        parsedSections =
            parseSections source

        ( evaluatedSections, functions ) =
            parsedSections
                |> List.foldr
                    (\item ( list, currentFunctions ) ->
                        let
                            ( value, maybeFunction ) =
                                evaluateSection item

                            newFunctions : IdDict FunctionName Function
                            newFunctions =
                                case maybeFunction of
                                    Just ( name, function ) ->
                                        IdDict.insert name function currentFunctions

                                    Nothing ->
                                        currentFunctions
                        in
                        ( value :: list, newFunctions )
                    )
                    ( [], IdDict.empty (\(FunctionName functionName) -> functionName) )

        --parsedSections |> List.map evaluateSection
    in
    case viewersError of
        Just error ->
            ( viewers, ErrorSection [ OutputError "\"viewers\" failed to evaluate:", error ] :: evaluatedSections, functions )

        Nothing ->
            ( viewers, evaluatedSections, functions )


makeHostViewers : Result x Env -> List (Value -> Maybe (Html.Html never))
makeHostViewers maybeEnv =
    let
        moduleName : String
        moduleName =
            String.join "" (Result.withDefault [] (Result.map .currentModule maybeEnv))
    in
    Dict.fromList Viewers.viewers
        |> Dict.get moduleName
        |> Maybe.withDefault []


makeFile : FullCode -> Result Error File
makeFile (FullCode source) =
    let
        file : Result (List DeadEnd) File
        file =
            Elm.Parser.parseToFile source
    in
    Result.mapError ParsingError file


sectionFromParsed : Result Error Env -> ( Code, Result error (List Cell) ) -> ( Section, Maybe ( FunctionName, Function ) )
sectionFromParsed maybeEnv ( source, parsedSection ) =
    case parsedSection of
        Ok cells ->
            let
                lastCode : Maybe Cell
                lastCode =
                    List.filter isCode cells
                        |> List.Extra.last

                isCode : Cell -> Bool
                isCode cell =
                    case cell of
                        CellDeclaration _ ->
                            True

                        _ ->
                            False

                isComment : Cell -> Bool
                isComment cell =
                    case cell of
                        CellComment _ ->
                            True

                        _ ->
                            False
            in
            case lastCode of
                Just (CellDeclaration (Node _ declaration)) ->
                    let
                        functionName : FunctionName
                        functionName =
                            extractNameFromDeclaration declaration |> FunctionName

                        evaluated : Result OutputError Value
                        evaluated =
                            evaluateName maybeEnv functionName
                    in
                    case evaluated of
                        Err error ->
                            ( EvaluatedSection source (Err error), Nothing )

                        Ok (PartiallyApplied ((PartiallyAppliedFunction _ alreadyApplied patterns _ _) as function)) ->
                            let
                                maybePairs : Result OutputError (List ( ParameterName, TypeName ))
                                maybePairs =
                                    parseTogether (patterns |> List.map Node.value) declaration (List.length alreadyApplied)
                            in
                            case maybePairs of
                                Err error ->
                                    ( EvaluatedSection source (Err error), Nothing )

                                Ok pairs ->
                                    ( InteractiveSection source functionName
                                    , Just
                                        ( functionName
                                        , { function = function
                                          , declaration = declaration
                                          , pairs = pairs
                                          }
                                        )
                                    )

                        Ok value ->
                            ( EvaluatedSection source (value |> OutputValue |> Ok), Nothing )

                _ ->
                    ( List.filter isComment cells
                        |> List.map
                            (\comment ->
                                case comment of
                                    CellComment (Node _ (Markdown text)) ->
                                        let
                                            contents =
                                                String.dropLeft 2 text
                                        in
                                        if String.startsWith " " contents then
                                            String.dropLeft 1 contents

                                        else
                                            contents

                                    _ ->
                                        ""
                            )
                        |> String.join "\n"
                        |> Markdown
                        |> MarkdownSection
                    , Nothing
                    )

        Err _ ->
            ( CodeSection source, Nothing )



-- EVALUATION


calculateOutput : Interactives -> IdDict FunctionName Function -> FunctionName -> Output
calculateOutput interactives functions functionName =
    case IdDict.get functionName functions of
        Just { function, declaration } ->
            applyPartiallyApplied interactives function declaration

        Nothing ->
            OutputError "No function stored with this name! This is an internal error." |> Err


applyPartiallyApplied : Interactives -> PartiallyAppliedFunction -> Declaration -> Output
applyPartiallyApplied evalInteractives partiallyApplied declaration =
    let
        (PartiallyAppliedFunction baseEnv alreadyApplied patterns _ expression) =
            partiallyApplied

        maybePairs : Result OutputError (List ( ParameterName, TypeName ))
        maybePairs =
            parseTogether (patterns |> List.map Node.value) declaration (List.length alreadyApplied)

        maybeValuePairs : Result OutputError (List ( ParameterName, Value ))
        maybeValuePairs =
            parseValuesTogether (patterns |> List.map Node.value) alreadyApplied

        functionName : FunctionName
        functionName =
            extractNameFromDeclaration declaration |> FunctionName

        insertIntoEnvFromValue : ( ParameterName, Value ) -> Dict String Value -> Dict String Value
        insertIntoEnvFromValue ( ParameterName binding, value ) localValues =
            Dict.insert binding value localValues

        insertIntoEnvFromType : ( ParameterName, TypeName ) -> Dict String Value -> Result OutputError (Dict String Value)
        insertIntoEnvFromType ( ParameterName binding, TypeName typeName ) localValues =
            let
                maybeTypeNode : Maybe InteractiveElement
                maybeTypeNode =
                    Dict.get typeName typeNodeMap

                maybeRawValue : Maybe RawInteractiveValue
                maybeRawValue =
                    Interactives.get ( functionName, ParameterName binding ) evalInteractives

                typeNodeToValue : InteractiveElement -> RawInteractiveValue -> Result OutputError Value
                typeNodeToValue typeNode rawValue =
                    typeNode.conversion rawValue

                maybeValue : Maybe (Result OutputError Value)
                maybeValue =
                    Maybe.map2 typeNodeToValue maybeTypeNode maybeRawValue
            in
            case maybeValue of
                Just (Ok value) ->
                    Dict.insert binding value localValues |> Ok

                _ ->
                    ("Missing \"" ++ binding ++ "\"") |> OutputError |> Err

        updateEnvFromValues : Env -> Env
        updateEnvFromValues env =
            case maybeValuePairs of
                Err _ ->
                    env

                Ok pairs ->
                    { env
                        | values =
                            List.foldl
                                insertIntoEnvFromValue
                                env.values
                                pairs
                    }

        updateEnvFromType : Env -> Result OutputError Env
        updateEnvFromType env =
            case maybePairs of
                Err _ ->
                    Ok env

                Ok pairs ->
                    case
                        Result.Extra.foldlWhileOk
                            insertIntoEnvFromType
                            env.values
                            pairs
                    of
                        Ok newValues ->
                            { env
                                | values = newValues
                            }
                                |> Ok

                        Err err ->
                            Err err

        functionOutput : Result OutputError Value
        functionOutput =
            (baseEnv |> updateEnvFromValues |> updateEnvFromType)
                |> Result.andThen (\env -> evaluate (Ok env) expression)
    in
    Result.map OutputValue functionOutput


evaluateName : Result Error Env -> FunctionName -> Result OutputError Value
evaluateName maybeEnv (FunctionName expressionName) =
    let
        expression : Expression
        expression =
            Elm.Syntax.Expression.FunctionOrValue
                []
                expressionName

        expressionNode : Node Expression
        expressionNode =
            Node.empty expression
    in
    evaluate maybeEnv expressionNode


evaluate : Result Error Env -> Node Expression -> Result OutputError Value
evaluate maybeEnv expressionNode =
    let
        moduleRun : Result Error Value
        moduleRun =
            case maybeEnv of
                Err e ->
                    Err e

                Ok env ->
                    let
                        ( result, _, _ ) =
                            Eval.Expression.evalExpression
                                expressionNode
                                { trace = False }
                                env
                    in
                    Result.mapError InterpreterTypes.EvalError result
    in
    moduleRun |> Result.mapError errorToString |> Result.mapError OutputError



-- PARSE TOGETHER


parseTogether : List Pattern -> Declaration -> Int -> Result OutputError (List ( ParameterName, TypeName ))
parseTogether patterns declaration numApplied =
    declarationTypeAnnotation declaration
        |> Result.map bigAnnotationToList
        |> Result.andThen
            (\ann ->
                List.map2 Tuple.pair (List.drop numApplied patterns) ann
                    |> Result.Extra.combineMap parseTogetherSingle
                    |> Result.map List.concat
            )


parseTogetherSingle : ( Pattern, TypeAnnotation ) -> Result OutputError (List ( ParameterName, TypeName ))
parseTogetherSingle ( pattern, annotation ) =
    case ( pattern, annotation ) of
        ( TuplePattern patterns, TypeAnnotation.Tupled annotations ) ->
            List.map2 Tuple.pair (patterns |> List.map Node.value) (annotations |> List.map Node.value)
                |> Result.Extra.combineMap parseTogetherSingle
                |> Result.map List.concat

        --( RecordPattern patterns, TypeAnnotation.Record record ) ->
        --    Err "record not implemented"
        --(UnConsPattern head rest, TypeAnnotation. )->
        --ListPattern (List (Node Pattern))
        --VarPattern String
        ( VarPattern binding, TypeAnnotation.Typed moduleNode patterns ) ->
            let
                typeName : String
                typeName =
                    moduleNode
                        |> Node.value
                        |> (\( moduleName, name ) -> moduleName ++ [ name ])
                        |> String.join "."

                fullTypeName : String
                fullTypeName =
                    typeName
                        :: (patterns
                                |> List.map Node.value
                                |> List.map annotationToString
                           )
                        |> String.join " "
            in
            Ok [ ( ParameterName binding, TypeName fullTypeName ) ]

        --NamedPattern QualifiedNameRef (List (Node Pattern))
        --AsPattern (Node Pattern) (Node String)
        --ParenthesizedPattern (Node Pattern)
        ( p, a ) ->
            Err (OutputError ("Can't handle variables of type " ++ (a |> annotationToString) ++ " with binding " ++ (p |> patternToString)))


parseValuesTogether : List Pattern -> List Value -> Result OutputError (List ( ParameterName, Value ))
parseValuesTogether patterns alreadyApplied =
    List.map2 Tuple.pair patterns alreadyApplied
        |> Result.Extra.combineMap parseValuesTogetherSingle
        |> Result.map List.concat


parseValuesTogetherSingle : ( Pattern, Value ) -> Result OutputError (List ( ParameterName, Value ))
parseValuesTogetherSingle ( pattern, rootValue ) =
    case ( pattern, rootValue ) of
        ( TuplePattern [ firstBinding, secondBinding ], Tuple first second ) ->
            [ ( Node.value firstBinding, first ), ( Node.value secondBinding, second ) ]
                |> Result.Extra.combineMap parseValuesTogetherSingle
                |> Result.map List.concat

        ( VarPattern binding, value ) ->
            Ok [ ( ParameterName binding, value ) ]

        ( p, v ) ->
            Err (OutputError ("Can't apply variables with value " ++ (v |> Value.toString) ++ " with binding " ++ (p |> patternToString)))



-- INTERACTIVE


viewInteractive : Interactives -> FunctionName -> ( ParameterName, TypeName ) -> Element FrontendMsg
viewInteractive interactives functionName ( (ParameterName bindingString) as binding, TypeName typeName ) =
    let
        maybeValue : Maybe RawInteractiveValue
        maybeValue =
            Interactives.get ( functionName, binding ) interactives
    in
    case Dict.get typeName typeNodeMap of
        Nothing ->
            viewOutputError (OutputError (bindingString ++ " - Interactive input of \"" ++ typeName ++ "\" not supported"))

        Just interactiveElement ->
            Element.Lazy.lazy3 interactiveElement.element functionName binding maybeValue


typeNodeMap : Dict String InteractiveElement
typeNodeMap =
    [ interactiveElementInt
    , interactiveElementChar
    , interactiveElementString
    ]
        |> List.map
            (\x ->
                ( let
                    (TypeName key) =
                        x.key
                  in
                  key
                , x
                )
            )
        |> Dict.fromList


type alias InteractiveElement =
    { key : TypeName
    , conversion : RawInteractiveValue -> Result OutputError Value
    , element : FunctionName -> ParameterName -> Maybe RawInteractiveValue -> Element FrontendMsg
    }


interactiveElementInt : InteractiveElement
interactiveElementInt =
    let
        conversion : RawInteractiveValue -> Result OutputError Value
        conversion (RawInteractiveValue text) =
            text
                |> String.toInt
                |> Result.fromMaybe "Couldn't parse!"
                |> Result.map Kernel.int.toValue
                |> Result.mapError OutputError
    in
    { key = TypeName "Int"
    , conversion = conversion
    , element = viewTextInput "Int"
    }


interactiveElementChar : InteractiveElement
interactiveElementChar =
    let
        conversion : RawInteractiveValue -> Result OutputError Value
        conversion (RawInteractiveValue text) =
            (if String.length text == 1 then
                String.toList text |> List.head

             else
                Nothing
            )
                |> Result.fromMaybe "Not exactly one character"
                |> Result.map Kernel.char.toValue
                |> Result.mapError OutputError
    in
    { key = TypeName "Char"
    , conversion = conversion
    , element = viewTextInput "Char"
    }


interactiveElementString : InteractiveElement
interactiveElementString =
    let
        conversion : RawInteractiveValue -> Result OutputError Value
        conversion (RawInteractiveValue text) =
            text
                |> Kernel.string.toValue
                |> Ok
    in
    { key = TypeName "String"
    , conversion = conversion
    , element = viewTextInput "String"
    }



-- UTILITIES


bigAnnotationToList : TypeAnnotation -> List TypeAnnotation
bigAnnotationToList annotation =
    case annotation of
        TypeAnnotation.FunctionTypeAnnotation first second ->
            (first
                |> Node.value
            )
                :: (second
                        |> Node.value
                        |> bigAnnotationToList
                   )

        other ->
            [ other ]


declarationTypeAnnotation : Declaration -> Result OutputError TypeAnnotation
declarationTypeAnnotation declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .signature
                |> Maybe.map Ok
                |> Maybe.withDefault (Err (OutputError "No type annotations found."))
                |> Result.map Node.value
                |> Result.map .typeAnnotation
                |> Result.map Node.value

        _ ->
            Err (OutputError "Not a function declaration")


extractNameFromDeclaration : Declaration -> String
extractNameFromDeclaration declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .declaration
                |> Node.value
                |> .name
                |> Node.value

        AliasDeclaration _ ->
            "typealias"

        CustomTypeDeclaration _ ->
            "customtype"

        PortDeclaration _ ->
            "portdeclaration"

        InfixDeclaration _ ->
            "infixdeclaration"

        Destructuring _ _ ->
            "destructuring"



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout []
            (Element.column
                -- left could be 185
                [ Element.alignLeft, Element.paddingEach { top = 40, left = 40, right = 40, bottom = 0 } ]
                (Element.el
                    [ Element.centerX

                    --, Font.typeface "IBM Plex Sans"
                    , Font.family
                        [ Font.typeface "Quicksand"
                        , Font.sansSerif
                        ]

                    --, Font.typeface [ "IBM Plex Sans", "sans-serif" ]
                    --, Font.family
                    --    [ Font.external
                    --        { name = "IBM Plex Sans"
                    --        , url = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,100..700;1,100..700&display=swap"
                    --        }
                    --    , Font.sansSerif
                    --    ]
                    , Font.size 72
                    , Font.color (Element.rgb255 120 120 120)
                    , Element.paddingEach { defaultPadding | bottom = 20 }
                    ]
                    (Element.text "elm-clerk")
                    :: viewError model.error
                    ++ (model.fileList |> List.map viewListItem)
                    ++ (case ( model.source, model.currentFileName, model.error ) of
                            ( Just source, _, _ ) ->
                                model.sections
                                    |> List.map
                                        (viewSection model.viewers
                                            model.hostViewers
                                            model.outputs
                                            model.functions
                                            model.interactives
                                        )

                            --[ Element.text "Source" ]
                            ( _, Nothing, _ ) ->
                                [ Element.el
                                    [ Font.family
                                        [ Font.typeface "Quicksand"
                                        , Font.sansSerif
                                        ]
                                    ]
                                    (Element.text
                                        (if List.length model.fileList >= 0 then
                                            "No file selected"

                                         else
                                            "No .elm files in the pages folder. Create one!"
                                        )
                                    )
                                ]

                            ( _, _, "" ) ->
                                [ Element.el
                                    [ Font.family
                                        [ Font.typeface "Quicksand"
                                        , Font.sansSerif
                                        ]
                                    ]
                                    (Element.text "Loading...")
                                ]

                            _ ->
                                []
                       )
                )
            )
        ]
    }


viewError : String -> List (Element FrontendMsg)
viewError error =
    case error of
        "" ->
            []

        _ ->
            String.split "\n" error |> List.map OutputError |> List.map viewOutputError


viewListItem : FileName -> Element FrontendMsg
viewListItem (FileName listItem) =
    Element.Input.button []
        { onPress = Just (ListItemClicked (FileName listItem))
        , label = Element.text listItem
        }


defaultPadding : { top : Int, right : Int, bottom : Int, left : Int }
defaultPadding =
    { top = 0, right = 0, bottom = 0, left = 0 }


viewChart : Element msg
viewChart =
    Element.el [ width maxWidth, Element.paddingEach { top = 40, left = 40, right = 40, bottom = 0 } ] <|
        Element.html <|
            C.chart
                [ CA.height 200
                , CA.width 300
                , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
                ]
                [ C.xLabels []
                , C.yLabels [ CA.withGrid ]
                , C.series .x
                    [ C.interpolated .y [ CA.monotone ] [ CA.circle ]
                    , C.interpolated .z [ CA.monotone ] [ CA.square ]
                    ]
                    [ { x = 1, y = 2, z = 3 }
                    , { x = 5, y = 4, z = 1 }
                    , { x = 10, y = 2, z = 4 }
                    ]
                ]


viewCode : Code -> Element msg
viewCode =
    Element.Lazy.lazy
        (\(Code code) ->
            Element.el [ width maxWidth ] <|
                Source.viewExpression [ scrollbarX, monospace ]
                    { highlight = Nothing
                    , buttons = []
                    , source = code
                    }
        )


viewSection : List Viewer -> List HostViewer -> IdDict FunctionName Output -> IdDict FunctionName Function -> Interactives -> Section -> Element FrontendMsg
viewSection viewers hostViewers outputs functions interactives section =
    let
        applyHostViewer : Value -> Maybe (Html.Html FrontendMsg)
        applyHostViewer value =
            List.Extra.stoppableFoldl
                (\hostViewer _ ->
                    case hostViewer value of
                        Just transformed ->
                            Stop (Just transformed)

                        Nothing ->
                            Continue Nothing
                )
                Nothing
                hostViewers

        applyViewer : Value -> Result OutputError (Maybe (Html.Html FrontendMsg))
        applyViewer value =
            let
                encoded =
                    Kernel.encodedValue.toValue value
            in
            List.Extra.stoppableFoldl
                (\viewer _ ->
                    case viewer encoded of
                        Ok Nothing ->
                            Continue (Ok Nothing)

                        Ok (Just transformed) ->
                            Stop (Ok (Just transformed))

                        Err error ->
                            Stop (Err error)
                )
                (Ok Nothing)
                viewers

        transform : Result OutputError OutputValue -> Result OutputError OutputValue
        transform valueResult =
            case valueResult of
                Err _ ->
                    valueResult

                Ok (OutputHtml _) ->
                    valueResult

                Ok (OutputValue value) ->
                    case applyViewer value of
                        Err error ->
                            Err error

                        Ok (Just html) ->
                            Ok (OutputHtml html)

                        Ok Nothing ->
                            case applyHostViewer value of
                                Just transformed ->
                                    Ok (OutputHtml transformed)

                                Nothing ->
                                    Ok (OutputValue value)
    in
    Element.column [ width fill, paddingEach { top = 10, right = 0, bottom = 0, left = 0 } ]
        (case section of
            MarkdownSection markdown ->
                [ viewMarkdownHtml markdown ]

            CodeSection code ->
                [ viewCode code ]

            EvaluatedSection code output ->
                [ viewCode code
                , case transform output of
                    Ok value ->
                        viewOutputValue value

                    Err value ->
                        viewOutputError value
                ]

            InteractiveSection code functionName ->
                [ viewCode code ]
                    ++ (case Maybe.map2 Tuple.pair (IdDict.get functionName outputs) (IdDict.get functionName functions) of
                            Nothing ->
                                [ viewOutputError (OutputError "Evaluating...") ]

                            Just ( output, function ) ->
                                [ Element.row
                                    [ width fill
                                    , Element.Background.color (Element.rgb255 240 240 240)
                                    , Element.paddingXY (graySidePadding - 9) 0
                                    ]
                                    (List.map
                                        (viewInteractive interactives functionName)
                                        function.pairs
                                    )
                                , case transform output of
                                    Ok value ->
                                        viewOutputValue value

                                    Err value ->
                                        viewOutputError value
                                ]
                       )

            ErrorSection error ->
                List.map viewOutputError error
        )


viewMarkdownHtml : Markdown -> Element FrontendMsg
viewMarkdownHtml =
    Element.Lazy.lazy
        (\markdown ->
            let
                markdownView : Markdown -> Result String (List (Html.Html msg))
                markdownView (Markdown localMarkdown) =
                    localMarkdown
                        |> Markdown.Parser.parse
                        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
                        |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
            in
            case markdownView markdown of
                Ok values ->
                    values
                        |> Html.div []
                        |> Element.html
                        |> Element.el
                            [ width maxWidth
                            , Font.family
                                [ Font.typeface "Fira Sans"
                                , Font.sansSerif
                                ]
                            ]

                Err err ->
                    Element.text err
        )


viewMarkdown : Markdown -> Element FrontendMsg
viewMarkdown markdown =
    let
        markdownView : Markdown -> Result String (List (Element msg))
        markdownView (Markdown localMarkdown) =
            localMarkdown
                |> Markdown.Parser.parse
                |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
                |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.ElmUi.renderer)
    in
    case markdownView markdown of
        Ok values ->
            values
                |> Element.column []

        Err err ->
            Element.text err


viewOutputValue : OutputValue -> Element FrontendMsg
viewOutputValue =
    Element.Lazy.lazy
        (\outputValue ->
            let
                viewHtml : Html.Html FrontendMsg -> Element FrontendMsg
                viewHtml html =
                    html
                        |> Element.html
                        |> Element.el [ Element.paddingEach { top = 12, right = graySidePadding, bottom = 4, left = graySidePadding } ]
            in
            case outputValue of
                OutputHtml html ->
                    viewHtml html

                OutputValue value ->
                    case Kernel.html.fromValue value of
                        Just html ->
                            html
                                |> Kernel.Html.htmlToReal
                                |> viewHtml

                        _ ->
                            viewOutput ("-> " ++ Value.toString value)
        )


viewOutputError : OutputError -> Element FrontendMsg
viewOutputError =
    Element.Lazy.lazy
        (\(OutputError output) ->
            viewOutput output
        )


viewOutput : String -> Element msg
viewOutput output =
    Element.el [ Element.paddingXY 0 3, width fill ] <|
        Element.el
            [ width <| maxWidth
            , height <| fill
            , Element.Background.color (Element.rgb255 240 240 240)
            , Element.paddingXY graySidePadding 8
            ]
        <|
            el
                [ width shrink
                , height <| maximum 300 shrink
                , scrollbarY
                , clipX
                , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                ]
                (paragraph [ monospace, Font.size 20 ] [ text output ])


viewTextInput : String -> FunctionName -> ParameterName -> Maybe RawInteractiveValue -> Element FrontendMsg
viewTextInput typeName functionName (ParameterName parameterName) maybeRawValue =
    let
        maybeValue : Maybe String
        maybeValue =
            maybeRawValue |> Maybe.map (\(RawInteractiveValue x) -> x)
    in
    Element.row [ Element.padding 6 ]
        [ Element.Input.text
            []
            { onChange = \x -> InteractiveUpdated ( functionName, ParameterName parameterName ) (RawInteractiveValue x)
            , text = Maybe.withDefault "" maybeValue
            , placeholder = Nothing
            , label =
                Element.Input.labelAbove [ monospace, Font.size 16, Element.paddingXY 6 0 ]
                    (Element.text (parameterName ++ " : " ++ typeName ++ " = " ++ Maybe.withDefault "" maybeValue))
            }
        ]


maxWidth : Element.Length
maxWidth =
    maximum 1000 fill


graySidePadding : Int
graySidePadding =
    14


monospace : Attribute msg
monospace =
    Font.family
        [ Font.typeface "Fira Code"
        , Font.monospace
        ]
