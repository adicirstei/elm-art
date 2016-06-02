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
import Maybe exposing(Maybe)

import ImageData


type alias Model =
  { palettes : List Palette, seed : Int, drawing: Maybe Drawing.Model}

view : Model -> Html Msg
view model =
  div []
  [ h1 [] [ text "Generative art with Elm" ]
  , div [] [ text "Seed:", text (toString model.seed)]
--  , div [] (List.map drawPalette model.palettes)
  , case model.drawing of
      Nothing -> div [] [text "still loading shit..."]
      Just dr -> Drawing.render dr
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

getRandomSeed : Cmd Msg
getRandomSeed = Random.generate Random (Random.int  0  999999)

init : (Model, Cmd Msg)
init = (Model [P.defaultPalette] 0 Nothing, getRandomSeed)

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
      PaletteLoadFail _ -> (model, Cmd.none)
      PaletteLoadSucceed lst ->
        let
          drw = Drawing.newDrawingModel model.seed lst
          cmd = ImageData.askImageData (Drawing.width, Drawing.height, drw.config.image)
        in ({model | palettes = lst, drawing = Just drw}, cmd)
      Frame _ ->
        case model.drawing of
          Nothing -> (model, Cmd.none)
          Just dr -> ({model | drawing = Just (Drawing.step dr)}, Cmd.none)
      Random seed -> ({model | seed = seed}, getPalettes)
      ImageData data ->
        case model.drawing of
          Nothing -> model ! []
          Just dr -> {model | drawing = Just { dr | imageMap = data }} ! []



subs : Model -> Sub Msg
subs model =
  let
    raf =
      case model.drawing of
        Nothing -> Sub.none
        Just dr -> if dr.config.steps < dr.step then Sub.none else AnimationFrame.diffs Frame
    imgData = ImageData.data ImageData
  in
    Sub.batch [raf, imgData]

main = Html.App.program
  { init = init
  , view = view
  , subscriptions = subs
  , update = update
  }
