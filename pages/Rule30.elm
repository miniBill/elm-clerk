module Rule30 exposing (..)

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



-- # Rule 30
-- This is a reimplementation of https://snapshots.nextjournal.com/clerk-demo/build/f8112d44fa742cd0913dcbd370919eca249cbcd9/notebooks/rule_30.html
-- as a homage and for testing capabilities
--
-- Let's explore cellular automata in an Elm Clerk Notebook.
--
-- We start by creating custom viewers for numbers, lists and lists of lists.
--
-- These viewers take the form of a function which takes a dynamic representation of
-- an elm structure and returns Just Html if it's a value the viewer can handle.
-- We iterate through the viewers, and the first one that returns a value is used,
-- which means you can distinguish based on value, not just type.
--
-- In this case we want to render 1's and 0's as white and black boxes,
-- lists of numbers as rows,
-- and lists of lists of numbers as several rows.
-- Let's test these!
-- Any simple declaration renders its value.


black : Int
black =
    0


white : Int
white =
    1


three : List Int
three =
    [ 1, 0, 1 ]


rows : List (List Int)
rows =
    [ [ 1, 1, 1 ]
    , [ 1, 0, 1 ]
    , [ 0, 0, 1 ]
    ]



-- Looks nice!
--
-- Functions with unapplied parameters give you fields to let you play with them
--
-- (Well, at least if the data type has been implemented yet :D)


either : Int -> Int -> List Int
either repetitions int =
    List.repeat repetitions int



-- _Rule 30_ is implemented as a set of rules for translating one state
-- to another, which can be represented as a case statement in elm.
-- The definition maps any vector of three cells to a new value for the middle cell.
-- Using a case lets the compiler guarantee that every combination has been defined
--
-- We later step through the state and apply this rule to each cell.


rule30 : List Int -> Int
rule30 list =
    case list of
        [ first, second, third ] ->
            case ( first, second, third ) of
                ( 1, 1, 1 ) ->
                    0

                ( 1, 1, 0 ) ->
                    0

                ( 1, 0, 1 ) ->
                    0

                ( 1, 0, 0 ) ->
                    1

                ( 0, 1, 1 ) ->
                    1

                ( 0, 1, 0 ) ->
                    1

                ( 0, 0, 1 ) ->
                    1

                ( 0, 0, 0 ) ->
                    0

        _ ->
            0



-- We can also render all states if we like


allRules : List (List Int)
allRules =
    List.Extra.cartesianProduct [ [ 1, 0 ], [ 1, 0 ], [ 1, 0 ] ]
        |> List.map
            (\list ->
                list ++ [ rule30 list ]
            )



-- Our first generation is a row with 33 elements, where the element in the center is a black square.


firstGeneration : List Int
firstGeneration =
    let
        n =
            33
    in
    List.repeat n 0 |> List.Extra.setAt (n // 2) 1



-- Finally, we can iterate over the first generation to evolve the state of the whole board over time.


evolvePreset : List (List Int)
evolvePreset =
    evolve 1



-- Try editing the "steps" parameter of evolve so it changes over time


evolve : Int -> List (List Int)
evolve steps =
    List.MyExtra.iterateN
        steps
        (\generation ->
            List.Extra.groupsOfWithStep 3 1 generation
                |> List.map rule30
                |> (\list -> [ 0 ] ++ list ++ [ 0 ])
        )
        firstGeneration
