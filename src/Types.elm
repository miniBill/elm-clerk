module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import FastDict as Dict exposing (Dict)
import Html exposing (Html)
import Http
import InterpreterTypes
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , source : String
    , sections : List Section
    , interactiveValues : InteractiveValues
    }


type alias InteractiveValues =
    Dict ( String, String ) String


type alias BackendModel =
    { message : String
    , placeholderOutput : InteractiveValues
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)
    | WroteText (Result Http.Error ())
    | InteractiveUpdated ( String, String ) String


type ToBackend
    = NoOpToBackend
    | OutputToBackend InteractiveValues


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type Section
    = MarkdownSection String
    | CodeSection String
    | EvaluatedSection String String
    | InteractiveSection String (List (Element FrontendMsg)) String
    | HtmlSection String (Html FrontendMsg)
    | ErrorSection (List String)
