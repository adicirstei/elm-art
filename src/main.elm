import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Json.Decode as Json
import Task
import Time exposing(Time)

import Html.App
import Http
import Color.Convert

import AnimationFrame
import Color
import Random

import Palette as P
import Drawing exposing(..)
import Types exposing(..)


view : Types.Model -> Html Msg
view model =
  let p = Maybe.withDefault defaultPalette (List.head model.palettes)
  in
    div []
    [ h1 [] [ text "Generative art with Elm" ]
    , div [] [ text "Seed:", text (toString model.seed)]
    , div [] (List.map drawPalette model.palettes)
    , render p
    ]


drawPalette p =
  div [ style [("margin", "2px"), ("float", "left")] ] (List.map colorDiv (P.toColorList p))


colorDiv color =
  div
  [ style
      [ ("background", Color.Convert.colorToHex color)
      , ("width", "20px")
      , ("height", "20px")
      , ("float", "left")
  ] ] []

init : (Types.Model, Cmd Msg)
init = (Types.Model [] 0, getRandomSeed)

subs : Types.Model -> Sub Msg
subs model =
  AnimationFrame.diffs Frame


defaultPalette : Palette
defaultPalette = Palette Color.black (RootElement Color.white )

getRandomSeed : Cmd Msg
getRandomSeed = Random.generate Random (Random.int 0 100)


getPalettes : Cmd Msg
getPalettes =
  Task.perform
    PaletteLoadFail
    PaletteLoadSucceed
    (Http.get P.decodePalettes "/data/palettes.json")


update : Msg -> Types.Model -> (Types.Model, Cmd Msg)
update msg model =
  case msg of
    Init -> (model, getPalettes)
    PaletteLoadFail _ -> (model, Cmd.none)
    PaletteLoadSucceed lst -> (Types.Model lst model.seed, Cmd.none)
    Frame dt -> (model, Cmd.none)
    Random seed -> (Types.Model model.palettes seed, getPalettes)

main = Html.App.program
  { init = init
  , view = view
  , subscriptions = subs
  , update = update
  }
