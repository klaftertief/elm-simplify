module Simplify.DouglasPeucker (run) where

{-| Douglas-Peucker polyline simplification

# Usage
@docs run

-}

import Array exposing (Array)
import Simplify.Util exposing (Point, farthestPoint, firstLast)


{-| Simplify a list of points using the Douglas-Peucker algorithm.

    runDouglasPeucker  1 longListOfPoints == shortListOfPoints
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
  case Array.isEmpty points of
    True -> points
    False ->
      let
        farthestPointState = farthestPoint points
      in
        if farthestPointState.distance > tolerance then
          let
            taken = Array.slice 0 (farthestPointState.index + 1) points
            dropped = Array.slice farthestPointState.index (Array.length points) points
          in
            Array.append
              (run' tolerance taken)
              -- TODO: remove first element
              (run' tolerance dropped)
        else
          let
            (first, last) = firstLast points
          in
            case (first, last) of
              (Just first, Just last) -> Array.fromList [first, last]
              _ -> points
