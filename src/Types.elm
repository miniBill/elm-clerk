module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import IntTypes
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , sources : List String
    , outputs : List Output
    }


type alias BackendModel =
    { message : String
    , sources : List String
    , outputs : List Output
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)


type ToBackend
    = NoOpToBackend
    | OutputToBackend (List String) (List Output)


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type alias Output =
    Result IntTypes.Error IntTypes.Value
