module Simplify (run) where

{-| An Elm port of [simplify-js](https://github.com/mourner/simplify-js), polyline simplification library by Vladimir Agafonkin.

# Usage
@docs run

-}

import Array exposing (Array)
import Simplify.DouglasPeucker as DouglasPeucker
import Simplify.RadialDistance as RadialDistance
import Simplify.Util exposing (Point)


{-| Simplify a list of points with a given tolerance, using the Radial Distance algorithm at first and then the Douglas-Peucker algorithm.

    run 1 [...] == [...]
-}
run : Float -> Array Point -> Array Point
run tolerance points =
  points
    |> RadialDistance.run tolerance
    |> DouglasPeucker.run tolerance
