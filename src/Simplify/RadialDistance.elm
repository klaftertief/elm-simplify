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
      --_ -> run' (abs tolerance) points
      _ -> run'' (abs tolerance) [] points


run' : Float -> List Point -> List Point
run' tolerance points =
  case points of
    [] -> []
    [x] -> points
    [x,y] -> points
    x::xs ->
      let
        isClose point = distance x point < tolerance
        tail = run' tolerance <| dropWhile isClose xs
        simplified =
          case tail of
            -- Make sure that the last element in the simplified list is always
            -- the last element from the original list.
            [] -> (List.reverse >> List.take 1) xs
            _ -> x :: tail
      in
        simplified

run'' : Float -> List Point -> List Point -> List Point
run'' tolerance accum points =
  case points of
    [] -> accum
    x::xs ->
      let
        isClose point = distance x point < tolerance
      in
        run''
          tolerance
          (List.append accum [x])
          (dropWhile isClose xs)
