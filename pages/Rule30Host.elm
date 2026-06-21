module Rule30Host exposing (..)

import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import InterpreterTypes exposing (Value)
import Kernel


viewers : List (Value -> Maybe (Html never))
viewers =
    let
        blockViewer : Int -> Html msg
        blockViewer number =
            let
                color =
                    if number > 0 then
                        "black"

                    else
                        "white"
            in
            Html.div
                [ style "display" "inline-block"
                , style "width" "16px"
                , style "height" "16px"
                , style "border-style" "solid"
                , style "border-color" "black"
                , style "border-width" "1.5px"
                , style "background-color" color
                ]
                []

        blockViewerRow : List Int -> Html msg
        blockViewerRow numbers =
            numbers
                |> List.map
                    blockViewer
                |> Html.div [ HA.style "float" "left" ]
    in
    [ \value ->
        Kernel.int.fromValue value
            |> Maybe.map
                blockViewer
    , \value ->
        (Kernel.list Kernel.int).fromValue value
            |> Maybe.map blockViewerRow
    , \value ->
        (Kernel.list (Kernel.list Kernel.int)).fromValue value
            |> Maybe.map
                (\lines ->
                    lines
                        |> List.map blockViewerRow
                        |> List.map List.singleton
                        |> List.map (Html.div [])
                        |> Html.div []
                )
    ]
