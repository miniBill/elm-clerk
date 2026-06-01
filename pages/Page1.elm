module Page1 exposing (output)

import Html exposing (Html)
import Html.Attributes
import Rope
import Types exposing (FrontendMsg)


myMaybe =
    Just 4


element : Html String
element =
    Html.div
        [ Html.Attributes.style "border-style" "solid"
        , Html.Attributes.style "border-width" "1px"
        ]
        [ Html.div []
            [ Html.text "a"
            ]
        , Html.div []
            [ Html.text "b"
            ]
        , Html.div []
            [ Html.text "c"
            ]
        ]



-- First block of code


output =
    List.sum (List.range 0 3)


add thing =
    thing + 5


added =
    add 11



-- Second block of code
-- which continues on the
-- next few rows!


a =
    11



-- A block with some _very_ important **MARKDOWN**
-- which lets us
-- # Establish
-- ## A hierarcy
-- of text


b =
    12


c =
    a + b


multiline =
    c + a


strings =
    String.concat (List.repeat 4 "abcdefg")


rope =
    Rope.empty
