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
      _ ->  run' (abs tolerance) Array.empty points


run' : Float  -> Array Point -> Array Point -> Array Point
run' tolerance accum points =
  let
    head = Array.get 0 points
    tail = Array.slice 1 (Array.length points) points
  in
    case head of
      Nothing -> accum
      Just x ->
        let
          isClose point = distance x point < tolerance
        in
          run'
            tolerance
            (Array.append accum (Array.fromList [x]))
            (dropWhile isClose tail)
