module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import IntTypes
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , source : String
    , sections : List Section
    }


type alias BackendModel =
    { message : String
    , placeholderOutput : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)
    | WroteText (Result Http.Error ())


type ToBackend
    = NoOpToBackend
    | OutputToBackend String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type Section
    = MarkdownSection String
    | CodeSection String
    | EvaluatedSection String String
    | ErrorSection (List String)
