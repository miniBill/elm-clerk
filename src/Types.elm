module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Node exposing (Node)
import FastDict as Dict exposing (Dict)
import Html exposing (Html)
import Http
import InterpreterTypes
import Parser exposing (DeadEnd)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , source : FullCode
    , parsedSections : List ( Code, ParsedSection )
    , interactives : Interactives
    }


type alias BackendModel =
    { message : String
    , interactives : Interactives
    }


type alias ParsedSection =
    Result (List DeadEnd) (List Cell)


type Cell
    = CellComment (Node Markdown)
    | CellDeclaration (Node Declaration)


type Interactives
    = Interactives (Dict ( String, String ) RawInteractiveValue)



-- Various types of string


type Markdown
    = Markdown String


type FullCode
    = FullCode String


type Code
    = Code String


type FunctionName
    = FunctionName String


type ParameterName
    = ParameterName String


type RawInteractiveValue
    = RawInteractiveValue String


type OutputError
    = OutputError String


type OutputValue
    = OutputValue String


type TypeName
    = TypeName String



-- Messages


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)
    | WroteText (Result Http.Error ())
    | InteractiveUpdated ( FunctionName, ParameterName ) RawInteractiveValue


type ToBackend
    = NoOpToBackend
    | InteractivesToBackend Interactives
    | RequestStartup


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | Startup Interactives


type Section
    = MarkdownSection Markdown
    | CodeSection Code
    | EvaluatedSection Code (Result OutputError OutputValue)
    | InteractiveSection Code (List (Element FrontendMsg)) (Result OutputError OutputValue)
    | HtmlSection Code (Html FrontendMsg)
    | ErrorSection (List OutputError)
