module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import IntTypes
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , sources : Sources
    , outputs : Outputs
    }


type alias BackendModel =
    { message : String
    , sources : Sources
    , outputs : Outputs
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)
    | WroteText (Result Http.Error ())


type ToBackend
    = NoOpToBackend
    | OutputToBackend Sources Outputs


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type alias Sources =
    List String


type alias Outputs =
    List (List String)



--type alias Output =
--    { module_run : Result IntTypes.Error IntTypes.Value
--    , section_parse : List String
--    }
