module Simplify.RadialDistance (run) where

{-| An Elm port of [simplify-js](https://github.com/mourner/simplify-js), polyline simplification library by Vladimir Agafonkin.

# Usage
@docs run

-}

import Simplify.Util exposing (Point, dropWhile, distance)


{-| Simplify a list of points with using the Radial Distance algorithm.

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
      _ -> run' (abs tolerance) points
      --x::xs ->
      --  let
      --    simplified = run' (abs tolerance) points
      --  in
      --    case simplified of
      --      -- We always want the first element, which might get swallowed during simplification
      --      --[s] -> x :: [s]
      --      _ -> simplified


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
        --x :: tail
