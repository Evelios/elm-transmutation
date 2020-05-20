module List.Util exposing (linspace, rotateLeft)

{-|


# List Utilities

@docs linspace, rotateLeft

-}


{-| Linearly space numbers from the beginning to the end. The list contains the number in the start range and in the
end range.
-}
linspace : Int -> Float -> Float -> List Float
linspace num from to =
    if from == to then
        List.repeat num from

    else
        let
            lower =
                min from to

            higher =
                max from to
        in
        List.range 0 (num - 1)
            |> List.map toFloat
            |> List.map (\n -> lower + ((n * (higher - lower)) / (toFloat num - 1)))


{-| Rotate a list where the head of the list is moved to the end of the list a given number of times.
-}
rotateLeft : Int -> List a -> List a
rotateLeft amount list =
    if amount <= 0 then
        list

    else
        case list of
            [] ->
                []

            x :: xs ->
                rotateLeft (amount - 1) (xs ++ [ x ])
