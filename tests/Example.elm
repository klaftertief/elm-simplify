import Html
import Html.Events exposing (onClick, on)
import Html.Attributes
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import StartApp.Simple as StartApp
import String

import Simplify
import Fixtures exposing (seventythousand)


model = 1

view address model =
  let
    simplified = Simplify.run model seventythousand
  in
    Html.div []
      [ Html.fromElement <| collage 800 500
        [ move (-600, -310)
          <| traced (solid blue)
          <| fromPoints simplified
        ]
      , Html.div
        [ Html.Attributes.style
          [ ("position", "absolute")
          , ("left", "100px")
          , ("top", "50px")
          , ("width", "500px")
          ]
        ]
        [ Html.h1 [] [ Html.text "Elm Simplify" ]
        , Html.input
          [ Html.Attributes.type' "range"
          , Html.Attributes.min "0"
          , Html.Attributes.max "5"
          , Html.Attributes.step "0.1"
          , Html.Attributes.value <| toString model
          , Html.Attributes.style [("width", "200px")]
          , on "input" Html.Events.targetValue (Signal.message address << FromString)
          ]
          [ ]
        , Html.p []
          [ Html.strong [] [ Html.text <| toString <| List.length seventythousand ]
          , Html.text " points simplified with a tolerance of "
          , Html.strong [] [ Html.text <| toString model ]
          , Html.text "px to "
          , Html.strong [] [ Html.text <| toString <| List.length simplified ]
          , Html.text " points."
          ]
        ]
      ]

fromPoints points =
  path <| List.map fromPoint points

fromPoint point =
  (point.x, point.y)


type Action
  = FromString String

update action model =
  case action of
    FromString value ->
      let
        parsed = String.toFloat value
      in
        case parsed of
          Ok float -> float
          Err error -> model


main =
  StartApp.start
  { model = model
  , view = view
  , update = update
  }

