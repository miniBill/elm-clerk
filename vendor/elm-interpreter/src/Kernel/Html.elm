module Kernel.Html exposing (..)

import Html
import Html.Attributes
import IntTypes exposing (Value(..))
import Json.Encode
import Value


type Html
    = Node String (List Attr) (List Html)
    | Text String


type Attr
    = Attribute String String
    | Property String Value


node : String -> List Attr -> List Html -> Html
node name attrs nodes =
    Node name attrs nodes


text : String -> Html
text string =
    Text string


style : String -> String -> Attr
style first second =
    Attribute first second


htmlToReal html =
    case html of
        Node name attrs children ->
            Html.node name
                (attrs |> List.map attrToReal)
                (children |> List.map htmlToReal)

        Text string ->
            Html.text string


attrToReal attr =
    case attr of
        Attribute first second ->
            Html.Attributes.style first second

        Property first (String second) ->
            Html.Attributes.property first (Json.Encode.string second)

        Property first second ->
            second
                |> Value.toString
                |> Json.Encode.string
                |> Html.Attributes.property first
