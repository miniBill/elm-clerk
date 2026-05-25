module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Elm.Syntax.Expression
import Eval
import Eval.Expression as EEval
import Eval.Module as MEval
import Html
import Html.Attributes as Attr
import Http
import Lamdera exposing (sendToBackend)
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
                        sources =
                            String.split "\n\n" fullText

                        outputs =
                            sources
                                |> List.map
                                    (\string ->
                                        MEval.eval string
                                            (Elm.Syntax.Expression.FunctionOrValue
                                                []
                                                "output"
                                            )
                                    )
                    in
                    ( { model | sources = sources, outputs = outputs }
                    , sendToBackend (OutputToBackend sources outputs)
                    )

                Err error ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text model.message ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Element.layout []
                    (Source.view []
                        { highlight = Nothing
                        , buttons = []
                        , source =
                            case List.head model.sources of
                                Just source ->
                                    source

                                Nothing ->
                                    ""
                        }
                    )
                ]
            , Html.div
                [ Attr.style "font-family" "monospace"
                , Attr.style "font-size" "40px"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text
                    (case List.head model.outputs of
                        Just output ->
                            case output of
                                Ok value ->
                                    Value.toString value

                                Err err ->
                                    "Error"

                        Nothing ->
                            "Not yet run"
                    )
                ]
            ]
        ]
    }
