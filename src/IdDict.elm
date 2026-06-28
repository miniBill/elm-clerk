module IdDict exposing (..)

import FastDict as Dict
import Types exposing (IdDict(..))


empty : (kind -> String) -> IdDict kind value
empty transform =
    IdDict Dict.empty transform


insert : kind -> value -> IdDict kind value -> IdDict kind value
insert key value (IdDict dict transform) =
    IdDict (Dict.insert (transform key) value dict) transform


get : kind -> IdDict kind value -> Maybe value
get key (IdDict dict transform) =
    Dict.get (transform key) dict


isEmpty : IdDict kind value -> Bool
isEmpty (IdDict dict _) =
    Dict.isEmpty dict


map : (String -> a -> b) -> IdDict k a -> IdDict k b
map func (IdDict dict transform) =
    IdDict (Dict.map func dict) transform


clear : IdDict kind value -> IdDict kind v
clear (IdDict _ transform) =
    IdDict Dict.empty transform
