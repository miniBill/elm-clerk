module Page1 exposing (output)

import Element
import Html exposing (Html)
import Html.Attributes exposing (style)
import Rope
import Types exposing (FrontendMsg)


repeatThrice : String -> String
repeatThrice =
    String.repeat 3

add : Int -> Int -> Int
add a b = a + b

increment : Int -> Int
increment = add 1