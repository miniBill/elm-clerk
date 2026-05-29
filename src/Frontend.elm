module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element
import Elm.Interface exposing (Exposed)
import Elm.Parser
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
      , sources = []
      , outputs = []
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
                Ok fullText ->
                    let
                        sources : Sources
                        sources =
                            [ fullText, fullText ]

                        module_run : String -> Result Error Value
                        module_run source =
                            Eval.Module.eval source
                                (Elm.Syntax.Expression.FunctionOrValue
                                    []
                                    "output"
                                )

                        parseOutput : List String
                        parseOutput =
                            runCustomParse fullText

                        outputs : Outputs
                        outputs =
                            --[ [ module_run fullText |> module_run_to_string ]
                            [ runCustom fullText
                            , parseOutput
                            ]
                    in
                    ( { model | sources = sources, outputs = outputs }
                    , Cmd.batch
                        [ sendToBackend (OutputToBackend sources outputs)
                        , Http.post
                            { url = "/_x/write/pages/Page1.elm.json"
                            , body = stringBody "application/json" (String.join "\n" parseOutput)
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


runCustomParse : String -> List String
runCustomParse source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            File.encode file
                |> Json.encode 2
                |> List.singleton

        Err error ->
            deadEndsToString error


runCustom : String -> List String
runCustom source =
    let
        expression : String -> Expression
        expression expressionName =
            Elm.Syntax.Expression.FunctionOrValue
                []
                expressionName

        file : Result (List DeadEnd) File
        file =
            Elm.Parser.parseToFile source

        --fileResult : Result (List DeadEnd) File
        --fileResult =
        --    ParserFast.run Elm.Parser.File.file source
        fileMappedError : Result Error File
        fileMappedError =
            Result.mapError ParsingError file

        maybeEnv : Result Error Env
        maybeEnv =
            Result.andThen Eval.Module.buildInitialEnv fileMappedError

        declarations : List String
        declarations =
            case fileMappedError of
                Ok fileLocal ->
                    fileLocal.declarations
                        |> List.map
                            (\node ->
                                let
                                    (Node range declaration) =
                                        node
                                in
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
                            )

                Err _ ->
                    []

        expressionNode : String -> Node Expression
        expressionNode expressionName =
            let
                expressionIndex : Maybe Int
                expressionIndex =
                    source
                        |> String.split "\n"
                        |> List.Extra.findIndex
                            (String.startsWith (expressionName ++ " ="))

                node : Node Expression
                node =
                    case expressionIndex of
                        Just index ->
                            Node
                                { start = { row = index + 1, column = 1 }
                                , end = { row = index + 1, column = 1 + String.length expressionName }
                                }
                                (expression expressionName)

                        Nothing ->
                            Node.empty (expression expressionName)
            in
            node

        module_run : String -> Result Error Value
        module_run expressionName =
            case maybeEnv of
                Err e ->
                    Err e

                Ok env ->
                    let
                        ( result, _, _ ) =
                            Eval.Expression.evalExpression
                                (expressionNode expressionName)
                                { trace = False }
                                env
                    in
                    Result.mapError IntTypes.EvalError result

        --module_run_output =
        --    module_run_to_string (module_run "output")
        evaluations : List String
        evaluations =
            declarations |> List.map (\name -> name ++ " = " ++ module_run_to_string (module_run name))
    in
    evaluations


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
                ++ List.map2 viewSection model.sources model.outputs
            )
        ]
    }


viewSection : String -> List String -> Html.Html FrontendMsg
viewSection source output =
    Html.div []
        ([ Html.div
            [ Attr.style "font-family" "sans-serif"
            , Attr.style "padding-top" "40px"
            ]
            [ Element.layout []
                (Source.view []
                    { highlight = Nothing
                    , buttons = []
                    , source = source
                    }
                )
            ]
         ]
            ++ List.map viewOutput output
        )


viewOutput : String -> Html.Html msg
viewOutput output =
    Html.div
        [ Attr.style "font-family" "monospace"
        , Attr.style "font-size" "40px"
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
