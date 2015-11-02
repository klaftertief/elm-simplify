module Simplify.RadialDistance (run) where

{-| Radial Distance polyline simplification

# Usage
@docs run

-}

import Simplify.Util exposing (Point, dropWhile, distance)


{-| Simplify a list of points using the Radial Distance algorithm.

    runRadialDistance 1 longListOfPoints == shortListOfPoints
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
      _ -> run' (abs tolerance) [] points

-- TODO: Don't forget to add the last point when needed

run' : Float -> List Point -> List Point -> List Point
run' tolerance accum points =
  case points of
    [] -> accum
    x::xs ->
      let
        isClose point = distance x point < tolerance
      in
        run'
          tolerance
          (List.append accum [x])
          (dropWhile isClose xs)
