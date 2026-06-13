module Page1 exposing (..)

import Element
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import InterpreterTypes exposing (Value)
import Kernel
import Rope
import Types exposing (FrontendMsg)
import Value


viewers : List ( String, Value -> Maybe (Html never) )
viewers =
    [ ( "Int"
      , \value ->
            Kernel.int.fromValue value
                |> Maybe.map
                    (\number ->
                        if number > 0 then
                            "white"

                        else
                            "black"
                    )
                |> Maybe.map
                    (\color ->
                        Html.div
                            [ style "display" "inline-block"
                            , style "width" "16px"
                            , style "height" "16px"
                            , style "border-style" "solid"
                            , style "border-color" "black"
                            , style "background-color" color
                            ]
                            []
                    )
      )
    ]


square : String -> Html never
square color =
    Html.div
        [ style "display" "inline-block"
        , style "width" "16px"
        , style "height" "16px"
        , style "border-style" "solid"
        , style "border-color" "black"
        , style "background-color" color
        ]
        []


blackSquare : Html never
blackSquare =
    square "black"


black : Int
black =
    0


white : Int
white =
    1


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


graphFunc : Int -> Int -> (Int -> b) -> List b
graphFunc start end func =
    List.range start end |> List.map func


powGraph : Int -> List Int
powGraph max =
    graphFunc 1 max pow
