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

type alias Model =
  { palettes : List Palette, seed : Int, drawing: Drawing.Model }

view : Model -> Html Msg
view model =
  let p = Maybe.withDefault defaultPalette (List.head model.palettes)
  in
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
init = (Model [defaultPalette] 0 (newDrawingModel 0 [defaultPalette]), getRandomSeed)

subs : Model -> Sub Msg
subs model =
  AnimationFrame.diffs Frame

newDrawingModel : Int -> List Palette -> Drawing.Model
newDrawingModel s lp =
  let
    seed = Random.initialSeed s
    (idx, sd) = Random.step (Random.int 0 (List.length lp)) seed
    p = List.drop idx lp
        |> List.head
        |> Maybe.withDefault defaultPalette
  in Drawing.Model p sd

defaultPalette : Palette
defaultPalette = Palette Color.black (RootElement Color.white )

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
  case msg of
    Init -> (model, getPalettes)
    PaletteLoadFail _ -> (model, Cmd.none)
    PaletteLoadSucceed lst -> (Model lst model.seed (newDrawingModel model.seed model.palettes), Cmd.none)
    Frame dt -> ({model | drawing = Drawing.step dt model.drawing}, Cmd.none)
    Random seed -> (Model model.palettes seed model.drawing, getPalettes)

main = Html.App.program
  { init = init
  , view = view
  , subscriptions = subs
  , update = update
  }
