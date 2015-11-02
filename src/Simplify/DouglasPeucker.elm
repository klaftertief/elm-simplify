module Simplify.DouglasPeucker (run) where

{-| Douglas-Peucker polyline simplification

# Usage
@docs run

-}

import Simplify.Util exposing (Point, farthestPoint, firstLast)


{-| Simplify a list of points using the Douglas-Peucker algorithm.

    runDouglasPeucker  1 longListOfPoints == shortListOfPoints
-}
run : Float -> List Point -> List Point
run tolerance points =
  if tolerance == 0 then
    points
  else
    case points of
      -- No simplification for short lists
      [] -> points
      [x] -> points
      [x,y] -> points
      _ -> run' (abs tolerance) points


run' : Float -> List Point -> List Point
run' tolerance points =
  case points of
    [] -> points
    _ ->
      let
        farthestPointState = farthestPoint points
      in
        if farthestPointState.distance > tolerance then
          let
            taken = List.take (farthestPointState.index + 1) points
            dropped = List.drop farthestPointState.index points
          in
            List.append
              (run' tolerance taken)
              (List.drop 1 <| run' tolerance dropped)
        else
          let
            (first, last) = firstLast points
          in
            case (first, last) of
              (Just first, Just last) -> [first, last]
              _ -> points
