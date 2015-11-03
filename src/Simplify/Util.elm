module Simplify.Util
  ( dropWhile
  , firstLast
  , Segment
  , Point
  , farthestPoint
  , distanceSegment
  , distance
  ) where


import Array exposing (Array)

dropWhile : (a -> Bool) -> Array a -> Array a
dropWhile predicate array =
  let
    head = Array.get 0 array
    tail = Array.slice 1 (Array.length array) array
  in
    case head of
      Nothing -> array
      Just point ->
        if predicate point then
          dropWhile predicate tail
        else
          array

firstLast : Array a -> (Maybe a, Maybe a)
firstLast array =
  let
    first = Array.get 0 array
    last = Array.get (Array.length array - 1) array
  in (first, last)

type alias Point =
  { x : Float
  , y : Float
  }

type alias Segment =
  ( Point
  , Point
  )

type alias FarthestPointState =
  { i : Int
  , index : Int
  , distance : Float
  , point : Maybe Point
  }

farthestPoint : Array Point -> FarthestPointState
farthestPoint points =
  let
    (first, last) = firstLast points
    state =
      { i = -1
      , index = 0
      , distance = 0
      , point = Nothing
      }
  in
    case (first, last) of
      (Just first, Just last) ->
        Array.foldl (farthestPoint' (first, last)) state points
      _ -> state

farthestPoint' : Segment -> Point -> FarthestPointState -> FarthestPointState
farthestPoint' segment point state =
  let
    distance = distanceSegment segment point
    state =
      { state | i = state.i + 1 }
  in
    if  distance > state.distance
      then
        { state
        | index = state.i
        , distance = distance
        , point = Just point
        }
      else
        state

distanceSegment : (Point, Point) -> Point -> Float
distanceSegment (s1, s2) p =
  let
    segmentLength = distance s1 s2
  in
    case segmentLength of
      0 -> distance s1 p
      _ ->
        let
          dx = (s2.x - s1.x)
          dy = (s2.y - s1.y)
          t = ((p.x - s1.x) * dx + (p.y - s1.y) * dy) / segmentLength ^ 2
          reference =
            if t < 0 then
              s1
            else if t > 1 then
              s2
            else
              { x = s1.x + t * dx
              , y = s1.y + t * dy
              }
        in
          distance reference p

distance : Point -> Point -> Float
distance p q =
  (p.x - q.x) ^ 2 + (p.y - q.y) ^ 2 |> sqrt

