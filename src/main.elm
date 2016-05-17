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

import Lists
import Palette as P
import Drawing exposing(..)

type alias Model =
  { palettes : List P.Palette, seed : Int }

type Msg
  = Init
  | PaletteLoadFail Http.Error
  | PaletteLoadSucceed (List P.Palette)
  | Frame Time
  | Random Int

view : Model -> Html Msg
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
  div [ style [("margin", "2px"), ("float", "left")] ] (List.map colorDiv (P.toList p))


colorDiv color =
  div
  [ style
      [ ("background", Color.Convert.colorToHex color)
      , ("width", "20px")
      , ("height", "20px")
      , ("float", "left")
  ] ] []

init : (Model, Cmd Msg)
init = (Model [] 0, getRandomSeed)

subs : Model -> Sub Msg
subs model =
  AnimationFrame.diffs Frame


defaultPalette : P.Palette
defaultPalette = P.Palette Color.black (Lists.RootElement Color.white )

getRandomSeed : Cmd Msg
getRandomSeed = Random.generate Random (Random.int 0 100)


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
    PaletteLoadSucceed lst -> (Model lst model.seed, Cmd.none)
    Frame dt -> (model, Cmd.none)
    Random seed -> (Model model.palettes seed, getPalettes)

main = Html.App.program
  { init = init
  , view = view
  , subscriptions = subs
  , update = update
  }
