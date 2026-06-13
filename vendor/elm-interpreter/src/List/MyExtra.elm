module List.MyExtra exposing (groupBy, iterateN)

import List.Extra


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy f list =
    list
        |> List.Extra.groupWhile (\l r -> f l == f r)
        |> List.map (\( head, tail ) -> ( f head, head :: tail ))


iterateN : Int -> (a -> a) -> a -> List a
iterateN n f x =
    iterateNHelp n f x [] |> List.reverse


iterateNHelp : Int -> (a -> a) -> a -> List a -> List a
iterateNHelp n f x acc =
    case n of
        0 ->
            acc

        _ ->
            iterateNHelp (n - 1) f (f x) (x :: acc)
