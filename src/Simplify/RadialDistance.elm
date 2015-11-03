module Simplify.RadialDistance (run) where

{-| Radial Distance polyline simplification

# Usage
@docs run

-}

import Array exposing (Array)
import Simplify.Util exposing (Point, dropWhile, distance)


{-| Simplify a list of points using the Radial Distance algorithm.

    runRadialDistance 1 longListOfPoints == shortListOfPoints
-}
run : Float -> Array Point -> Array Point
run tolerance points =
  if tolerance == 0 then
    points
  else
    case Array.length points of
      -- No simplification for short lists
      0 -> points
      1 -> points
      2 -> points
      _ -> run' (abs tolerance) points


run' : Float -> Array Point -> Array Point
run' tolerance points =
  let
    head = Array.get 0 points
    tail = Array.slice 1 (Array.length points) points
  in
    case head of
      Nothing -> points
      Just point ->
        let
          isClose point = distance point point < tolerance
        in
          Array.append
            (Array.fromList [point])
            (run' tolerance <| dropWhile isClose tail)
