module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)

import Color

art palette =
  let
    (bg, fg) =
      case palette of
        hd::tl -> (hd, tl)
        _ -> (Color.black, [Color.white])
    myLine = { defaultLine | width = 4.5, cap = Round, join = Smooth, color = Maybe.withDefault Color.white (List.head fg) }
  in
    collage 900 600
      [ rect 900 600 |> filled bg
      , segment (-10.0, -20.9) (30.0,50.8) |> traced myLine]

draw palette = toHtml (art palette)
