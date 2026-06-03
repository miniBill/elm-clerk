module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Attribute, Element, fill, paddingEach, px, width)
import Element.Background
import Element.Font as Font
import Elm.Parser
import Elm.Parser.Comments
import Elm.Parser.Declarations
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Eval.Expression
import Eval.Module
import Html
import Http exposing (stringBody)
import IntTypes exposing (CallTree, Env, Error(..), Value(..))
import Kernel
import Kernel.Html
import Lamdera exposing (sendToBackend)
import List.Extra
import Markdown.Parser
import Markdown.Renderer
import Markdown.Renderer.ElmUi
import Parser exposing (DeadEnd)
import ParserFast
import ParserWithComments exposing (WithComments)
import Regex
import Result.Extra
import ToString exposing (annotationToString, annotationToStringsDebug, deadEndsToStrings, evalErrorKindToString, functionDeclarationToString, patternToString, patternToStringDebug)
import Types exposing (..)
import UI.Source as Source
import Url
import Value


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding! "
      , source = ""
      , sections = []
      , interactiveValues = Dict.empty
      }
    , Http.get
        { url = "/_x/read/pages/Page1.elm"
        , expect = Http.expectString GotText
        }
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

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotText result ->
            case result of
                Ok source ->
                    let
                        sections : List Section
                        sections =
                            sectionsFromSource model source
                    in
                    ( { model | sections = sections }
                    , Cmd.batch
                        [ sendToBackend (OutputToBackend "placeholder")
                        , Http.post
                            { url = "/_x/write/pages/Page1.elm.txt"
                            , body = stringBody "application/text" (plaintextFromSections sections)
                            , expect = Http.expectWhatever WroteText
                            }
                        ]
                    )

                Err error ->
                    ( model, Cmd.none )

        WroteText _ ->
            ( model, Cmd.none )

        InteractiveUpdated name value ->
            ( { model | interactiveValues = Dict.insert name value model.interactiveValues }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )



-- PARSING AND EVALUATION


parse : String -> List String
parse source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            [ "Ok" ]

        Err deadEnds ->
            deadEndsToStrings deadEnds


sectionsFromSource : Model -> String -> List Section
sectionsFromSource model source =
    let
        newSectionRegex : Regex.Regex
        newSectionRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "\n\n+(?=[^\\s])"

        maybeEnv : Result Error Env
        maybeEnv =
            makeEnv source
    in
    Regex.split newSectionRegex
        source
        --|> List.map (\x -> "\"" ++ x ++ "\"")
        |> List.map (parseSection model maybeEnv)
        |> (\list ->
                case maybeEnv of
                    Err (ParsingError deadEnds) ->
                        ErrorSection (deadEndsToStrings deadEnds) :: list

                    _ ->
                        list
           )


plaintextFromSections : List Section -> String
plaintextFromSections sections =
    ""


type Cell
    = CellComment (Node String)
    | CellDeclaration (Node Declaration)


parseSection : FrontendModel -> Result Error Env -> String -> Section
parseSection model maybeEnv source =
    let
        parser : ParserFast.Parser (List Cell)
        parser =
            ParserFast.skipWhileWhitespaceFollowedBy
                (ParserFast.loopWhileSucceeds
                    (ParserFast.oneOf2
                        (ParserFast.map (\x -> CellDeclaration x.syntax) Elm.Parser.Declarations.declaration)
                        (ParserFast.map CellComment Elm.Parser.Comments.singleLineComment)
                        |> ParserFast.followedBySkipWhileWhitespace
                    )
                    []
                    (\cell list -> cell :: list)
                    (\list -> List.reverse list)
                )

        output : Result (List DeadEnd) (List Cell)
        output =
            ParserFast.run parser source

        isCode cell =
            case cell of
                CellDeclaration _ ->
                    True

                _ ->
                    False

        isComment cell =
            case cell of
                CellComment _ ->
                    True

                _ ->
                    False

        handleOutput result =
            case result of
                Ok cells ->
                    let
                        lastCode =
                            List.filter isCode cells
                                |> List.Extra.last
                    in
                    case lastCode of
                        Just (CellDeclaration (Node _ declaration)) ->
                            let
                                expressionName =
                                    extractNameFromDeclaration declaration

                                evaluated : Result String Value
                                evaluated =
                                    evaluateName maybeEnv expressionName
                            in
                            case evaluated of
                                Err error ->
                                    EvaluatedSection source error

                                Ok ((PartiallyApplied _ _ _ _ _) as functionDeclaration) ->
                                    handlePartiallyApplied source functionDeclaration declaration

                                Ok value ->
                                    handleSuccessfulParse source value

                        --EvaluatedSection source evaluated
                        _ ->
                            List.filter isComment cells
                                |> List.map
                                    (\comment ->
                                        case comment of
                                            CellComment (Node _ text) ->
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
                                |> MarkdownSection

                Err _ ->
                    CodeSection source
    in
    handleOutput output


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


declarationTypeAnnotation : Declaration -> Result String TypeAnnotation
declarationTypeAnnotation declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .signature
                |> Maybe.map Ok
                |> Maybe.withDefault (Err "No signature")
                |> Result.map Node.value
                |> Result.map .typeAnnotation
                |> Result.map Node.value

        _ ->
            Err "Not a function declaration"


parseTogether : List Pattern -> Declaration -> Int -> Result String (List ( String, String ))
parseTogether patterns declaration numApplied =
    declarationTypeAnnotation declaration
        |> Result.map bigAnnotationToList
        |> Result.andThen
            (\ann ->
                List.map2 Tuple.pair (List.drop numApplied patterns) ann
                    |> Result.Extra.combineMap parseTogetherSingle
                    |> Result.map List.concat
            )


parseTogetherSingle : ( Pattern, TypeAnnotation ) -> Result String (List ( String, String ))
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
            Ok [ ( binding, fullTypeName ) ]

        --NamedPattern QualifiedNameRef (List (Node Pattern))
        --AsPattern (Node Pattern) (Node String)
        --ParenthesizedPattern (Node Pattern)
        ( p, a ) ->
            Err ("Can't handle " ++ patternToStringDebug p ++ " : " ++ (a |> annotationToString))


parseTogetherExperiment : Declaration -> List Pattern -> List a -> String
parseTogetherExperiment declaration evalPatterns alreadyApplied =
    let
        arguments : Maybe (List Pattern)
        arguments =
            declarationArguments declaration

        bigAnnotation : Result String TypeAnnotation
        bigAnnotation =
            declarationTypeAnnotation declaration

        relevantPatterns : List String
        relevantPatterns =
            evalPatterns
                |> List.drop (List.length alreadyApplied)
                |> List.map patternToString

        annotationList : List TypeAnnotation
        annotationList =
            bigAnnotation
                |> Result.map bigAnnotationToList
                |> Result.withDefault []

        parsedTogether : Result String (List ( String, String ))
        parsedTogether =
            parseTogether
                evalPatterns
                declaration
                (List.length alreadyApplied)
    in
    (if Maybe.withDefault [] arguments == evalPatterns then
        "Equal"

     else
        "Not equal"
    )
        ++ "\n"
        ++ (if List.length (parsedTogether |> Result.withDefault []) == List.length annotationList - 1 then
                "Annotations seem correct"

            else
                "Annotations seem wrong"
           )
        ++ "\n Patterns from Eval:\n"
        ++ (evalPatterns
                |> List.map patternToString
                |> String.join ", "
           )
        ++ "\n Patterns from syntax:\n"
        ++ (arguments
                |> Maybe.withDefault []
                |> List.map patternToString
                |> String.join ", "
           )
        ++ "\n Type annotations:\n"
        ++ (annotationList
                |> List.map annotationToString
                |> String.join ", "
           )
        --++ "\n Pairs: \n"
        --++ (parse
        --        |> List.map (\( first, second ) -> first ++ ": " ++ second)
        --        |> String.join "\n"
        --   )
        ++ "\n Parsed together: \n"
        ++ (case parsedTogether of
                Ok list ->
                    list
                        |> List.map (\( pattern, annotation ) -> pattern ++ ": " ++ annotation)
                        |> String.join "\n"

                Err string ->
                    "Error: " ++ string
           )


handlePartiallyApplied : String -> Value -> Declaration -> Section
handlePartiallyApplied source functionDeclaration declaration =
    case functionDeclaration of
        PartiallyApplied env values patterns maybeName expression ->
            let
                together =
                    parseTogetherExperiment declaration (patterns |> List.map Node.value) values

                --togetherTwo =
                --    parseTogetherTwo declaration (patterns |> List.map Node.value) (List.length values)
                --
                parameterNames : List String
                parameterNames =
                    [ "first", "second" ]

                functionOutput : Result String Value
                functionOutput =
                    --let
                    --zipped = List.map2 Tuple.pair parameterNames model.interactiveValues
                    --mapFunction key value =
                    --    value |> List.map
                    --newEnv : Env
                    --newEnv =
                    --    { env
                    --        | values = Dict.union (model.interactiveValues |> Dict.map (\key value ->
                    --            let a : FrontendModel -> MaybeEnv -> String -> Section
                    --            a = value in
                    --, interactiveValues : Dict String (List IntTypes.Value)
                    --                                                        a
                    --                                                    ) env.values
                    --                                                }
                    --                                        in
                    evaluate (Ok env) expression

                debugElements : List (Element msg)
                debugElements =
                    [ functionDeclaration
                        |> functionDeclarationToString
                        |> viewOutput
                    , viewOutput together
                    ]
            in
            case functionOutput of
                Ok functionOutputOk ->
                    --EvaluatedSection source (functionDeclarationToString functionDeclaration)
                    InteractiveSection source
                        debugElements
                        (Value.toString functionOutputOk)

                Err functionOutputError ->
                    --EvaluatedSection source (functionDeclarationToString functionDeclaration)
                    InteractiveSection source debugElements functionOutputError

        _ ->
            ErrorSection [ "Called handlePartiallyApplied with a declaration that was not a function" ]


handleSuccessfulParse source value =
    case Kernel.html.fromValue value of
        Just html ->
            Kernel.Html.htmlToReal html
                |> HtmlSection source

        _ ->
            EvaluatedSection source (Value.toString value)


makeEnv : String -> Result Error Env
makeEnv source =
    let
        file : Result (List DeadEnd) File
        file =
            Elm.Parser.parseToFile source

        fileMappedError : Result Error File
        fileMappedError =
            Result.mapError ParsingError file

        maybeEnv : Result Error Env
        maybeEnv =
            Result.andThen Eval.Module.buildInitialEnv fileMappedError
    in
    maybeEnv


evaluateName : Result Error Env -> String -> Result String Value
evaluateName maybeEnv expressionName =
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


evaluate : Result Error Env -> Node Expression -> Result String Value
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
                    Result.mapError IntTypes.EvalError result

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
                        ++ [ evalErrorKindToString errorData.error ]
                        |> String.join "\n"
    in
    module_run |> Result.mapError error_to_string


extractNameFromDeclaration : Declaration -> String
extractNameFromDeclaration declaration =
    case declaration of
        FunctionDeclaration function ->
            let
                expressionName : String
                expressionName =
                    function
                        |> .declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            expressionName

        AliasDeclaration typeAlias ->
            "typealias"

        CustomTypeDeclaration type_ ->
            "customtype"

        PortDeclaration signature ->
            "portdeclaration"

        InfixDeclaration infix_ ->
            "infixdeclaration"

        Destructuring node1 node2 ->
            "destructuring"



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        --[ Html.div [ Attr.style "text-align" "left", Attr.style "padding-top" "40px", Attr.style "padding-left" "185px", Attr.style "padding-right" "40px" ]
        --([ Html.div [ Attr.style "text-align" "center" ]
        --    [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
        --    , Html.div
        --        [ Attr.style "font-family" "sans-serif"
        --        , Attr.style "padding-top" "40px"
        --        ]
        --        [ Html.text model.message ]
        --    ]
        -- ]
        [ Element.layout []
            (Element.column
                -- left could be 185
                [ Element.alignLeft, Element.paddingEach { top = 40, left = 40, right = 40, bottom = 0 } ]
                ([ Element.image [ width (px 150), Element.centerX ] { src = "https://lamdera.app/lamdera-logo-black.png", description = "Lamdera logo" } ]
                    ++ List.map viewSection model.sections
                )
            )
        ]

    --)
    --]
    }


monospace : Attribute msg
monospace =
    Font.family
        [ Font.typeface "Fira Code"
        , Font.monospace
        ]


viewSection : Section -> Element FrontendMsg
viewSection section =
    let
        syntaxHighlight : String -> Element msg
        syntaxHighlight code =
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
                [ syntaxHighlight code, viewOutput output ]

            InteractiveSection code elements output ->
                [ syntaxHighlight code, Element.row [ width fill ] elements, viewOutput output ]

            HtmlSection code html ->
                [ syntaxHighlight code, Element.html html ]

            ErrorSection strings ->
                List.map viewOutput strings
        )


viewMarkdownHtml : String -> Element msg
viewMarkdownHtml markdown =
    let
        markdownView : String -> Result String (List (Html.Html msg))
        markdownView localMarkdown =
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


viewMarkdown : String -> Element FrontendMsg
viewMarkdown markdown =
    let
        markdownView : String -> Result String (List (Element msg))
        markdownView localMarkdown =
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


viewOutput : String -> Element msg
viewOutput output =
    Element.column
        [ monospace
        , Font.size 24
        , Element.paddingXY 10 10
        , Element.Background.color (Element.rgb255 240 240 240)
        , width fill
        ]
        [ Element.text output ]
        |> Element.el [ Element.paddingXY 0 3, width fill ]
