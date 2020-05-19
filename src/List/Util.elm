module List.Util exposing (linspace)

{-|


# List Utilities

@docs linspace

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
