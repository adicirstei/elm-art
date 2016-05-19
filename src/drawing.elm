module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)
import Random
import Color
import Time
import Noise

import Types exposing(..)
import Palette

type alias Particle =
  { time : Time.Time }



type alias Model =
  { palette : Palette
  , seed : Random.Seed
  , particles : List Particle
  , pointilism : Float
  , table : Noise.PermutationTable
  }


newDrawingModel : Int -> List Palette -> Model
newDrawingModel s lp =
  let
    seed = Random.initialSeed s
    (idx, s1) = Random.step (Random.int 0 (List.length lp)) seed
    pl = List.drop idx lp
        |> List.head
        |> Maybe.withDefault Palette.defaultPalette
    (pt, s2) = Random.step (Random.float 0.0 0.1) s1
    (table, s3) = Noise.permutationTable s2
  in Model pl s3 [] pt table

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
