module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)
import Random
import Color
import Time

import Lists exposing(..)
import Palette

type Msg
  = Init Palette.Palette Random.Seed
  | Step Time.Time


type alias Model =
  { palette : Palette.Palette
  , seed : Random.Seed
  }

init : Palette.Palette -> Int -> (Model, Cmd Msg)
init p s =
  (Model p (Random.initialSeed s), Cmd.none)


art palette  =
  let
    (bg, fg) = (palette.bg, palette.fg)

    myLine = { defaultLine | width = 4.5, cap = Round, join = Smooth, color = head fg }
  in
    collage 900 600
      [ rect 900 600 |> filled bg
      , segment (-10.0, -20.9) (30.0,50.8) |> traced myLine]

render = art >> toHtml
