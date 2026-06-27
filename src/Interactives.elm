module Interactives exposing (..)

import FastDict as Dict
import Types exposing (FunctionName(..), Interactives(..), ParameterName(..), RawInteractiveValue(..))


empty : Interactives
empty =
    Interactives Dict.empty


insert : ( FunctionName, ParameterName ) -> RawInteractiveValue -> Interactives -> Interactives
insert ( FunctionName functionName, ParameterName parameterName ) value (Interactives interactives) =
    Interactives (Dict.insert ( functionName, parameterName ) value interactives)


get : ( FunctionName, ParameterName ) -> Interactives -> Maybe RawInteractiveValue
get ( FunctionName functionName, ParameterName parameterName ) (Interactives interactives) =
    Dict.get ( functionName, parameterName ) interactives



--interactivesFromList : List ( FunctionName, ParameterName ) -> Interactives
--interactivesFromList list =
--    let
--        unpack : ( FunctionName, ParameterName ) -> RawInteractiveValue -> ( ( String, String ), RawInteractiveValue )
--        unpack ( FunctionName functionName, ParameterName parameterName ) value =
--            ( ( functionName, parameterName ), value )
--    in
--    List.map unpack list
--        |> Dict.fromList
--        |> Interactives
