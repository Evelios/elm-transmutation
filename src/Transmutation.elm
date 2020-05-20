module Transmutation exposing (Transmutation, cross, geometry)

{-|


# Types

@ docs Transmutation


# Accessors

@ geometry


# Transmutation functions

@ cross

-}

import Geometry exposing (Geometry(..))
import LineSegment2d
import List.Extra
import List.Util
import Point2d exposing (Point2d)
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



-------- Accessors


{-| Get the geometry from this level of the transmutation.
-}
geometry : Transmutation units coordinates -> List (Geometry units coordinates)
geometry transmutation =
    case transmutation of
        Terminal theGeometry ->
            theGeometry

        _ ->
            []



-------- Types


cross : RegularPolygon2d units coordinates -> Transmutation units coordinates
cross polygon =
    let
        midpoints =
            RegularPolygon2d.midpoints polygon
                |> List.Util.rotateLeft (RegularPolygon2d.sides polygon // 2)

        internalGeometry =
            List.Extra.zip (RegularPolygon2d.vertices polygon) midpoints
                |> List.map LineSegment2d.fromEndpoints
                |> List.map Line
    in
    terminal internalGeometry
