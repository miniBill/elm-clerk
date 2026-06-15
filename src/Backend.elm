module Backend exposing (Model, app)

import FastDict as Dict
import Interactives exposing (interactivesEmpty)
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Types exposing (BackendModel, BackendMsg(..), FrontendMsg(..), ToBackend(..), ToFrontend(..))


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , interactives = interactivesEmpty
      , scroll = 0
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        InteractivesToBackend interactives ->
            ( { model | interactives = interactives }, Cmd.none )

        RequestStartup ->
            ( model
            , sendToFrontend clientId
                (Startup
                    { interactives = model.interactives
                    , scroll = model.scroll
                    }
                )
            )

        NewScrollToBackend y ->
            ( { model | scroll = y }, Cmd.none )
