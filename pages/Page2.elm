module Page2 exposing (..)

import Element
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import InterpreterTypes exposing (Value)
import Kernel
import List.Extra
import List.MyExtra
import Rope
import Types exposing (FrontendMsg)
import Value


double : Int -> Int
double x =
    2 * x
