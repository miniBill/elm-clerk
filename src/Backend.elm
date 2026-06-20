module Backend exposing (Model, app)

import Common exposing (notifyIn)
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
      , checksum = ""
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        RequestNewSource clientId ->
            ( model, sendToFrontend clientId RequestNewSourceToFrontend )


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
                    , checksum = model.checksum
                    }
                )
            )

        NewScrollToBackend y ->
            ( { model | scroll = y }, Cmd.none )

        NewChecksumToBackend newChecksum ->
            --( model, Cmd.none )
            ( { model | checksum = newChecksum }, notifyIn (RequestNewSource clientId) 900 )



--( { model | checksum = newChecksum }, sendToFrontend clientId RequestNewSourceToFrontend )
