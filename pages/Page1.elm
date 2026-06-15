module Page1 exposing (..)

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


repeat : Char -> Char -> Int -> String -> String
repeat start end repetitions text =
    String.fromChar start ++ String.repeat repetitions text ++ String.fromChar end


repeatParen : Int -> String -> String
repeatParen =
    repeat '(' ')'


a : Int
a =
    1


b : Int
b =
    2


c : Int
c =
    3


d : Int
d =
    4


e : Int
e =
    5


f : Int
f =
    6


g : Int
g =
    7
