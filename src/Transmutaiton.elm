module Transmutaiton exposing (Transmutation)

{-| -}

import Geometry exposing (Geometry)
import RegularPolygon2d exposing (RegularPolygon2d)


{-| -}
type Transmutation units coordinates
    = Terminal (List (Geometry units coordinates))
    | Transmutation (List (Geometry units coordinates)) (Node units coordinates) (List (Node units coordinates))


type Node units coordinates
    = Node (RegularPolygon2d units coordinates) (Transmutation units coordinates)



-------- Building


terminal : List (Geometry units coordinates) -> Transmutation units coordinates
terminal =
    Terminal


fork :
    { geometry : List (Geometry units coordinates)
    , external : List (Transmutation units coordinates)
    }
    -> Transmutation units coordinates
fork _ =
    terminal []



-------- Types
