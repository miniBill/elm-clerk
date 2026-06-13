module Page1 exposing (..)

import Element
import Html exposing (Html)
import Html.Attributes exposing (style)
import Rope
import Types exposing (FrontendMsg)


repeatThrice : String -> String
repeatThrice =
    String.repeat 3


repeat : Int -> String -> String
repeat =
    String.repeat


add : Int -> Int -> Int
add a b =
    a + b


increment : Int -> Int
increment =
    add 1


pow : Int -> Int
pow x =
    x * x


graph : Int -> Int -> (Int -> b) -> List b
graph start end func =
    List.range start end |> List.map func


powGraph : Int -> List Int
powGraph max =
    graph 1 max pow
