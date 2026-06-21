module Viewers exposing (..)

import Html exposing (Html)
import InterpreterTypes exposing (Value)
import Rule30Host


viewers : List ( String, List (Value -> Maybe (Html never)) )
viewers =
    [ ( "Rule30", Rule30Host.viewers ) ]
