module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Node exposing (Node)
import FastDict as Dict exposing (Dict)
import Html exposing (Html)
import Http
import InterpreterTypes exposing (PartiallyAppliedFunction, Value)
import Lamdera exposing (ClientId)
import Parser exposing (DeadEnd)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , currentFileName : Maybe FileName
    , source : Maybe FullCode
    , checksum : Maybe String
    , error : String
    , fileList : List FileName
    , sections : List Section
    , inputInteractives : Interactives
    , evalInteractives : Interactives
    , functions : Dict String Function
    , viewers : List Viewer
    , hostViewers : List HostViewer
    , outputs : Dict String Output
    }


type alias BackendModel =
    { message : String
    , interactives : Interactives
    , scroll : Float
    , checksum : String
    , fileName : Maybe FileName
    }


type alias Function =
    { function : PartiallyAppliedFunction
    , declaration : Declaration
    , pairs : List ( ParameterName, TypeName )
    }


type alias Output =
    Result OutputError OutputValue


type alias ParsedSection =
    Result (List DeadEnd) (List Cell)


type alias Viewer =
    Value -> Result OutputError (Maybe (Html.Html FrontendMsg))


type alias HostViewer =
    Value -> Maybe (Html.Html FrontendMsg)


type Cell
    = CellComment (Node Markdown)
    | CellDeclaration (Node Declaration)


{-| Dictionary of (FunctionName,ParameterName) to user-provided value for that param
-}
type Interactives
    = Interactives (Dict ( String, String ) RawInteractiveValue)



--type IdDict
--    = IdDict (Dict ( String, String ) RawInteractiveValue)
-- Various types of string


{-| To be rendered as markdown
-}
type Markdown
    = Markdown String


{-| The full code of a module
-}
type FullCode
    = FullCode String


{-| A snippet of code
-}
type Code
    = Code String


{-| The name of an interactive function
-}
type FunctionName
    = FunctionName String


{-| The name of a parameter of an interactive function
-}
type ParameterName
    = ParameterName String


{-| What the user puts into a field underneath a function, before validation or parsing
-}
type RawInteractiveValue
    = RawInteractiveValue String


{-| Marked for being output in a monospace section
-}
type OutputError
    = OutputError String


{-| The result of an interactive function
-}
type OutputValue
    = OutputValue Value
    | OutputHtml (Html.Html FrontendMsg)


{-| A type name from parsing type annotations
-}
type TypeName
    = TypeName String


{-| The name of a file in the pages folder, including the .elm extension
-}
type FileName
    = FileName String



-- Messages


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)
    | GotList (Result Http.Error (List String))
    | WroteText (Result Http.Error ())
    | ListItemClicked FileName
    | InteractiveUpdated ( FunctionName, ParameterName ) RawInteractiveValue
    | CheckGenerateOutputs
    | ReloadCode
    | Poll
    | NewScroll Float
    | NoOp


type ToBackend
    = NoOpToBackend
    | InteractivesToBackend Interactives
    | NewScrollToBackend Float
    | RequestStartup
    | NewChecksumToBackend String
    | NewFilenameToBackend FileName


type BackendMsg
    = NoOpBackendMsg
    | RequestNewSource ClientId
    | RequestNewFileName ClientId FileName


type ToFrontend
    = NoOpToFrontend
    | Startup { interactives : Interactives, scroll : Float, checksum : String, fileName : Maybe FileName }
    | RequestNewSourceToFrontend
    | RequestNewFileNameToFrontend FileName


type Section
    = MarkdownSection Markdown
    | CodeSection Code
    | EvaluatedSection Code (Result OutputError OutputValue)
    | InteractiveSection Code FunctionName
    | ErrorSection (List OutputError)
