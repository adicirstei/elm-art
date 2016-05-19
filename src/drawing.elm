module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)
import Random
import Color
import Time

import Types exposing(..)
import Palette


type alias Model =
  { palette : Palette
  , seed : Random.Seed
  }


art model  =
  let
    (bg, fg) = (model.palette.bg, model.palette.fg)

    myLine = { defaultLine | width = 4.5, cap = Round, join = Smooth, color = head fg }
  in
    collage 900 600
      [ rect 900 600 |> filled bg
      , segment (-10.0, -20.9) (30.0,50.8) |> traced myLine]

render = art >> toHtml

step : Time.Time -> Model -> Model
step dt model =
  model
