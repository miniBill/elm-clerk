module Frontend exposing (Model, app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Element exposing (Attribute, Element, clipX, el, fill, height, maximum, minimum, paddingEach, paragraph, px, scrollbarY, shrink, text, width)
import Element.Background
import Element.Font as Font
import Element.Input
import Element.Lazy
import Elm.Parser
import Elm.Parser.Comments
import Elm.Parser.Declarations
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Eval.Expression
import Eval.Module
import FastDict as Dict exposing (Dict)
import Html
import Html.Attributes
import Http
import Interactives exposing (interactivesEmpty, interactivesGet, interactivesInsert)
import InterpreterTypes exposing (Env, Error(..), Value(..))
import Kernel
import Kernel.Html
import Lamdera exposing (sendToBackend)
import List.Extra
import Markdown.Parser
import Markdown.Renderer
import Markdown.Renderer.ElmUi
import Parser exposing (DeadEnd)
import ParserFast
import Process
import Regex
import Result.Extra
import Task
import ToString exposing (annotationToString, deadEndsToStrings, evalErrorKindToString, patternToString)
import Types exposing (BackendMsg(..), Cell(..), Code(..), FrontendModel, FrontendMsg(..), FullCode(..), FunctionName(..), Interactives(..), Markdown(..), OutputError(..), OutputValue(..), ParameterName(..), ParsedSection, RawInteractiveValue(..), Section(..), ToBackend(..), ToFrontend(..), TypeName(..))
import UI.Source as Source
import Url
import Value



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
      , source = FullCode ""
      , parsedSections = []
      , evalInteractives = interactivesEmpty
      , inputInteractives = interactivesEmpty
      }
    , Cmd.batch
        [ Http.get
            { url = "/_x/read/pages/Page1.elm"
            , expect = Http.expectString GotText
            }
        , sendToBackend RequestStartup
        ]
    )


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
                    ( { model | source = FullCode source, parsedSections = parseSections (FullCode source) }
                    , Cmd.none
                      --[ sendToBackend (OutputToBackend "placeholder")
                      --[ Http.post
                      --    { url = "/_x/write/pages/Page1.elm.txt"
                      --    , body = stringBody "application/text" (plaintextFromSections sectionResults)
                      --    , expect = Http.expectWhatever WroteText
                      --    }
                      --]
                    )

                Err _ ->
                    ( model, Cmd.none )

        WroteText _ ->
            ( model, Cmd.none )

        InteractiveUpdated names value ->
            let
                newInteractives =
                    interactivesInsert names value model.inputInteractives
            in
            ( { model | inputInteractives = newInteractives }
            , Cmd.batch
                [ sendToBackend (InteractivesToBackend newInteractives)
                , notifyIn ReloadCode 10
                ]
            )

        ReloadCode ->
            ( { model | evalInteractives = model.inputInteractives }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        Startup interactives ->
            ( { model | inputInteractives = interactives, evalInteractives = interactives }, Cmd.none )


notifyIn : FrontendMsg -> Float -> Cmd FrontendMsg
notifyIn msg time =
    Process.sleep time
        |> Task.attempt (\_ -> msg)



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



-- SECTIONS EVALUATION


evaluateSections : Model -> List Section
evaluateSections model =
    let
        maybeEnv : Result Error Env
        maybeEnv =
            makeEnv model.source

        evaluateSection sectionResult =
            sectionResult
                |> sectionFromParsed model.evalInteractives model.inputInteractives maybeEnv
    in
    model.parsedSections |> List.map evaluateSection


makeEnv : FullCode -> Result Error Env
makeEnv (FullCode source) =
    let
        file : Result (List DeadEnd) File
        file =
            Elm.Parser.parseToFile source

        fileMappedError : Result Error File
        fileMappedError =
            Result.mapError ParsingError file
    in
    Result.andThen Eval.Module.buildInitialEnv fileMappedError


sectionFromParsed : Interactives -> Interactives -> Result Error Env -> ( Code, Result error (List Cell) ) -> Section
sectionFromParsed evalInteractives inputInteractives maybeEnv ( source, parsedSection ) =
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

                handleSuccessfulParse : Code -> Value -> Section
                handleSuccessfulParse localSource value =
                    case Kernel.html.fromValue value of
                        Just html ->
                            Kernel.Html.htmlToReal html
                                |> HtmlSection localSource

                        _ ->
                            EvaluatedSection localSource (Value.toString value |> OutputValue |> Ok)
            in
            case lastCode of
                Just (CellDeclaration (Node _ declaration)) ->
                    let
                        expressionName : FunctionName
                        expressionName =
                            extractNameFromDeclaration declaration |> FunctionName

                        evaluated : Result OutputError Value
                        evaluated =
                            evaluateName maybeEnv expressionName
                    in
                    case evaluated of
                        Err error ->
                            EvaluatedSection source (Err error)

                        Ok ((PartiallyApplied _ _ _ _ _) as functionDeclaration) ->
                            handlePartiallyApplied evalInteractives inputInteractives source functionDeclaration declaration

                        Ok value ->
                            handleSuccessfulParse source value

                --EvaluatedSection source evaluated
                _ ->
                    List.filter isComment cells
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

        Err _ ->
            CodeSection source



-- EVALUATION


handlePartiallyApplied : Interactives -> Interactives -> Code -> Value -> Declaration -> Section
handlePartiallyApplied evalInteractives inputInteractives source partiallyApplied declaration =
    case partiallyApplied of
        PartiallyApplied baseEnv alreadyApplied patterns _ expression ->
            let
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
                            interactivesGet ( functionName, ParameterName binding ) evalInteractives

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

                interactiveElements : Result OutputError (List (Element FrontendMsg))
                interactiveElements =
                    case ( maybePairs, maybeValuePairs ) of
                        ( Ok pairs, Ok _ ) ->
                            pairs
                                |> Result.Extra.combineMap (viewInteractive inputInteractives functionName)
                                |> Result.Extra.extract (\error -> viewOutputError error |> List.singleton)
                                |> Ok

                        ( Err error, _ ) ->
                            error
                                |> Err

                        ( _, Err error ) ->
                            error
                                |> Err
            in
            case interactiveElements of
                Err output ->
                    EvaluatedSection source (Err output)

                Ok elements ->
                    case functionOutput of
                        Ok functionOutputOk ->
                            InteractiveSection source
                                elements
                                (Value.toString functionOutputOk |> OutputValue |> Ok)

                        Err functionOutputError ->
                            InteractiveSection source elements (Err functionOutputError)

        _ ->
            ErrorSection [ OutputError "Called handlePartiallyApplied with a declaration that was not a function" ]


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
        module_run : Result Error Value
        module_run =
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

        error_to_string : Error -> String
        error_to_string error =
            case error of
                ParsingError deadends ->
                    deadEndsToStrings deadends
                        |> String.join "\n"

                EvalError errorData ->
                    (errorData.callStack
                        |> List.map
                            (\nameRef -> String.join "." (nameRef.moduleName ++ [ nameRef.name ]))
                    )
                        ++ [ "" ]
                        ++ [ evalErrorKindToString errorData.error ]
                        |> (\list -> "Call stack:" :: list)
                        |> String.join "\n"
    in
    module_run |> Result.mapError error_to_string |> Result.mapError OutputError



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


viewInteractive : Interactives -> FunctionName -> ( ParameterName, TypeName ) -> Result OutputError (Element FrontendMsg)
viewInteractive interactiveValues functionName ( binding, TypeName typeName ) =
    let
        maybeValue =
            interactivesGet ( functionName, binding ) interactiveValues

        (ParameterName bindingString) =
            binding
    in
    case Dict.get typeName typeNodeMap of
        Nothing ->
            Err (OutputError (bindingString ++ " - No way to handle type \"" ++ typeName ++ "\""))

        Just interactiveElement ->
            Ok (interactiveElement.element ( functionName, binding ) maybeValue)


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
    , element : ( FunctionName, ParameterName ) -> Maybe RawInteractiveValue -> Element FrontendMsg
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


declarationArguments : Declaration -> Maybe (List Pattern)
declarationArguments declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .declaration
                |> Node.value
                |> .arguments
                |> List.map Node.value
                |> Just

        _ ->
            Nothing



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
                    , Font.family
                        [ Font.external
                            { name = "IBM Plex Sans"
                            , url = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,100..700;1,100..700&display=swap"
                            }
                        , Font.sansSerif
                        ]
                    , Font.size 72
                    , Font.color (Element.rgb255 120 120 120)
                    ]
                    (Element.text "elm-clerk")
                    :: (evaluateSections model
                            |> List.map (Element.Lazy.lazy viewSection)
                       )
                    ++ [ viewChart ]
                )
            )
        ]
    }


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


viewSection : Section -> Element FrontendMsg
viewSection section =
    let
        syntaxHighlight : Code -> Element msg
        syntaxHighlight (Code code) =
            Source.viewExpression []
                { highlight = Nothing
                , buttons = []
                , source = code
                }
    in
    --Html.div []
    --    [ Html.div
    --        [ Attr.style "font-family" "sans-serif"
    --        , Attr.style "padding-top" "10px"
    --        ]
    Element.column [ width fill, paddingEach { top = 10, right = 0, bottom = 0, left = 0 } ]
        (case section of
            MarkdownSection markdown ->
                [ viewMarkdownHtml markdown ]

            CodeSection code ->
                [ syntaxHighlight code ]

            EvaluatedSection code output ->
                [ syntaxHighlight code
                , case output of
                    Ok value ->
                        viewOutputValue value

                    Err value ->
                        viewOutputError value
                ]

            InteractiveSection code elements output ->
                [ syntaxHighlight code
                , Element.row
                    [ width fill
                    , Element.Background.color (Element.rgb255 240 240 240)
                    , Element.paddingXY (graySidePadding - 9) 0
                    ]
                    elements
                , case output of
                    Ok value ->
                        viewOutputValue value

                    Err value ->
                        viewOutputError value
                ]

            HtmlSection code html ->
                [ syntaxHighlight code, Element.html html ]

            ErrorSection error ->
                List.map viewOutputError error
        )


viewMarkdownHtml : Markdown -> Element FrontendMsg
viewMarkdownHtml markdown =
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

        Err err ->
            Element.text err


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
viewOutputValue (OutputValue output) =
    viewOutput ("-> " ++ output)


viewOutputError : OutputError -> Element FrontendMsg
viewOutputError (OutputError output) =
    viewOutput output


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


viewTextInput : String -> ( FunctionName, ParameterName ) -> Maybe RawInteractiveValue -> Element FrontendMsg
viewTextInput typeName ( functionName, ParameterName parameterName ) maybeRawValue =
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
