module Simplify.RadialDistance (run) where

{-| Radial Distance polyline simplification

# Usage
@docs run

-}

import Simplify.Util exposing (Point, dropWhile, distance)
import Trampoline exposing (Trampoline, trampoline)

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
      _ -> trampoline <| run' (abs tolerance) points


run' : Float -> List Point -> Trampoline (List Point)
run' tolerance points =
  case points of
    [] -> Trampoline.Done []
    [x] -> Trampoline.Done points
    [x,y] -> Trampoline.Done points
    x::xs ->
      let
        isClose point = distance x point < tolerance
      in
        -- TODO: how to add `x` to the list?
        Trampoline.Continue (\_ -> (run' tolerance <| dropWhile isClose xs))
