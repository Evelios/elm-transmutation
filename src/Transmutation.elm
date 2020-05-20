module Transmutation exposing (Transmutation, cross, getGeometry, vertexFork)

{-|


# Types

@ docs Transmutation


# Accessors

@ getGeometry


# Transmutation functions

@ cross

-}

import Circle2d
import Geometry exposing (Geometry(..))
import LineSegment2d
import List.Extra
import List.Util
import Point2d exposing (Point2d)
import Quantity
import RegularPolygon2d exposing (RegularPolygon2d)


{-| -}
type Transmutation units coordinates
    = Terminal (List (Geometry units coordinates))
    | Fork (List (Geometry units coordinates)) (List (Transmutation units coordinates))



-------- Building


terminal : List (Geometry units coordinates) -> Transmutation units coordinates
terminal =
    Terminal


fork :
    { geometry : List (Geometry units coordinates)
    , external : List (Transmutation units coordinates)
    }
    -> Transmutation units coordinates
fork { geometry, external } =
    Fork geometry external



-------- Accessors


{-| Get the geometry from this level of the transmutation.
-}
getGeometry : Transmutation units coordinates -> List (Geometry units coordinates)
getGeometry transmutation =
    case transmutation of
        Terminal geometry ->
            geometry

        Fork geometry forkTransmutation ->
            List.append geometry <| List.concat <| List.map getGeometry forkTransmutation



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


vertexFork : RegularPolygon2d units coordinates -> Transmutation units coordinates
vertexFork polygon =
    let
        createPolygon vertex =
            RegularPolygon2d.fromUnsafe
                { sides = RegularPolygon2d.sides polygon
                , radius = Quantity.half (RegularPolygon2d.edgeLength polygon)
                , center = vertex
                , angle = RegularPolygon2d.angle polygon
                }

        continuations =
            RegularPolygon2d.vertices polygon
                |> List.map createPolygon
    in
    fork
        { external = List.map cross continuations
        , geometry = List.map RegularPolygon continuations
        }
