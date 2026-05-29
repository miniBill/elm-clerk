module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element
import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Parser.Comments
import Elm.Parser.Declarations
import Elm.Parser.File
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File as File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Eval
import Eval.Expression
import Eval.Module
import Html
import Html.Attributes as Attr
import Http exposing (stringBody)
import IntTypes exposing (CallTree, Env, Error(..), Value)
import Json.Encode as Json
import Lamdera exposing (sendToBackend)
import List.Extra
import Parser exposing (DeadEnd)
import ParserFast
import ParserWithComments exposing (WithComments)
import Regex
import Rope exposing (Rope)
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
                            sectionsFromSource source
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


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


parse : String -> List String
parse source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            [ "Ok" ]

        Err deadEnds ->
            deadEndsToString deadEnds


sectionsFromSource : String -> List Section
sectionsFromSource source =
    let
        newSectionRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "\n\n+(?=[^\\s])"

        maybeEnv =
            makeEnv source
    in
    Regex.split newSectionRegex
        source
        --|> List.map (\x -> "\"" ++ x ++ "\"")
        |> List.map (parseSection maybeEnv)


plaintextFromSections : List Section -> String
plaintextFromSections sections =
    ""


type Cell
    = CellComment (Node String)
    | CellDeclaration (Node Declaration)


parseSection : MaybeEnv -> String -> Section
parseSection maybeEnv source =
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
                                name =
                                    extractNameFromDeclaration declaration
                            in
                            EvaluatedSection source (evaluate maybeEnv name)

                        _ ->
                            let
                                firstComment =
                                    List.filter isComment cells |> List.head
                            in
                            case firstComment of
                                Just (CellComment (Node _ text)) ->
                                    let
                                        contents =
                                            String.dropLeft 2 text
                                    in
                                    if String.startsWith " " contents then
                                        MarkdownSection (String.dropLeft 1 contents)

                                    else
                                        MarkdownSection contents

                                _ ->
                                    CodeSection source

                Err err ->
                    CodeSection source
    in
    handleOutput output


type alias MaybeEnv =
    Result Error Env


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


evaluate : Result Error Env -> String -> String
evaluate maybeEnv expressionName =
    let
        expression : Expression
        expression =
            Elm.Syntax.Expression.FunctionOrValue
                []
                expressionName

        expressionNode : Node Expression
        expressionNode =
            Node
                { start = { row = 1, column = 1 }
                , end = { row = 1, column = 2 }
                }
                expression

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
    in
    expressionName ++ " = " ++ module_run_to_string module_run


extractNameFromDeclaration : Declaration -> String
extractNameFromDeclaration declaration =
    case declaration of
        FunctionDeclaration function ->
            let
                (Node _ impl) =
                    function.declaration

                (Node _ name) =
                    impl.name
            in
            name

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


deadEndsToString : List DeadEnd -> List String
deadEndsToString deadEnds =
    deadEnds
        |> List.map
            (\deadEnd ->
                "At row "
                    ++ String.fromInt deadEnd.row
                    ++ ", column "
                    ++ String.fromInt deadEnd.col
                    ++ ", problem : "
                    ++ (case deadEnd.problem of
                            Parser.Expecting string ->
                                "Expecting " ++ string

                            Parser.ExpectingInt ->
                                "Expecting Int"

                            Parser.ExpectingHex ->
                                "Expecting hex"

                            Parser.ExpectingOctal ->
                                "Expecting Octal"

                            Parser.ExpectingBinary ->
                                "Expecting Binary"

                            Parser.ExpectingFloat ->
                                "Expecting Float"

                            Parser.ExpectingNumber ->
                                "Expecting Number"

                            Parser.ExpectingVariable ->
                                "Expecting Variable"

                            Parser.ExpectingSymbol string ->
                                "Expecting symbol " ++ string

                            Parser.ExpectingKeyword string ->
                                "Expecting keyword " ++ string

                            Parser.ExpectingEnd ->
                                "Expecting end"

                            Parser.UnexpectedChar ->
                                "Unexpected char"

                            Parser.Problem string ->
                                "Problem: " ++ string

                            Parser.BadRepeat ->
                                "BadRepeat"
                       )
            )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "left", Attr.style "padding-top" "40px", Attr.style "padding-left" "185px", Attr.style "padding-right" "40px" ]
            ([ Html.div [ Attr.style "text-align" "center" ]
                [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
                , Html.div
                    [ Attr.style "font-family" "sans-serif"
                    , Attr.style "padding-top" "40px"
                    ]
                    [ Html.text model.message ]
                ]
             ]
                ++ List.map viewSection model.sections
            )
        ]
    }


viewSection : Section -> Html.Html FrontendMsg
viewSection section =
    let
        syntaxHighlight : String -> Html.Html msg
        syntaxHighlight code =
            Element.layout []
                (Source.view []
                    { highlight = Nothing
                    , buttons = []
                    , source = code
                    }
                )
    in
    Html.div []
        [ Html.div
            [ Attr.style "font-family" "sans-serif"
            , Attr.style "padding-top" "10px"
            ]
            (case section of
                MarkdownSection markdown ->
                    [ viewOutput markdown ]

                CodeSection code ->
                    [ syntaxHighlight code ]

                EvaluatedSection code output ->
                    [ syntaxHighlight code, viewOutput output ]
            )
        ]


viewOutput : String -> Html.Html msg
viewOutput output =
    Html.div
        [ Attr.style "font-family" "monospace"
        , Attr.style "font-size" "20px"
        ]
        [ Html.pre []
            [ Html.text output
            ]
        ]


module_run_to_string : Result Error Value -> String
module_run_to_string output =
    case output of
        Ok value ->
            Value.toString value

        Err _ ->
            "Error"
