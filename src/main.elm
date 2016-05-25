import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Html.Lazy
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

type alias Model =
  { palettes : List Palette, seed : Int, drawing: Drawing.Model, step : Int }

view : Model -> Html Msg
view model =
  div []
  [ h1 [] [ text "Generative art with Elm" ]
  , div [] [ text "Seed:", text (toString model.seed)]
  , div [] (List.map drawPalette model.palettes)
  , Drawing.render model.drawing
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

init : (Model, Cmd Msg)
init = (Model [P.defaultPalette] 0 (Drawing.newDrawingModel 0 [P.defaultPalette]) 0, getRandomSeed)

subs : Model -> Sub Msg
subs model =
  if model.drawing.config.steps < model.step
    then Sub.none
    else AnimationFrame.diffs Frame

getRandomSeed : Cmd Msg
getRandomSeed = Random.generate Random (Random.int 0 Random.maxInt)

getPalettes : Cmd Msg
getPalettes =
  Task.perform
    PaletteLoadFail
    PaletteLoadSucceed
    (Http.get P.decodePalettes "/data/palettes.json")


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let x = 100
  in
    case msg of
      Init -> (model, getPalettes)
      PaletteLoadFail _ -> (model, Cmd.none)
      PaletteLoadSucceed lst -> (Model lst model.seed (Drawing.newDrawingModel model.seed lst) 0, Cmd.none)
      Frame dt -> ({model | drawing = Drawing.step dt model.drawing, step = model.step + 1}, Cmd.none)
      Random seed -> (Model model.palettes seed model.drawing 0, getPalettes)

main = Html.App.program
  { init = init
  , view = Html.Lazy.lazy view
  , subscriptions = subs
  , update = update
  }
