module Page1 exposing (output)

import Element
import Html exposing (Html)
import Html.Attributes exposing (style)
import Rope
import Types exposing (FrontendMsg)


add thing =
    thing + 5


myMaybe =
    Just 4


html : Html String
html =
    Html.div
        []
        [ Html.div
            [ style "border-style" "solid"
            , style "border-width" "1px"
            ]
            [ Html.text "a"
            ]
        , Html.div
            [ style "border-style" "solid"
            , style "border-width" "1px"
            ]
            [ Html.text "b"
            ]
        , Html.div
            [ style "border-style" "solid"
            , style "border-width" "1px"
            ]
            [ Html.text "c"
            ]
        ]


element : Html String
element =
    Element.layout []
        (Element.column
            []
            [ Element.text "a"
            , Element.text "b"
            , Element.text "c"
            ]
        )



-- First block of code


output =
    List.sum (List.range 0 3)


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
