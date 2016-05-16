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

import Palette exposing(..)
import Drawing exposing(..)

type alias Model =
  { palettes : List Palette }

type Msg
  = Init
  | PaletteLoadFail Http.Error
  | PaletteLoadSucceed (List Palette)
  | Frame Time

view : Model -> Html Msg
view model =
  let p = Maybe.withDefault [] (List.head model.palettes)
  in
    div []
    [ h1 [] [ text "Generative art with Elm" ]
    , div [] [ text (toString (List.length model.palettes))]
    , div [] (List.map drawPalette model.palettes)
    , draw p
    ]


drawPalette p =
  div [ style [("margin", "2px"), ("float", "left")] ] (List.map colorDiv p)


colorDiv color =
  div
  [ style
      [ ("background", Color.Convert.colorToHex color)
      , ("width", "20px")
      , ("height", "20px")
      , ("float", "left")
  ] ] []

init : (Model, Cmd Msg)
init = (Model [], getPalettes)

subs : Model -> Sub Msg
subs model =
  AnimationFrame.diffs Frame


getPalettes : Cmd Msg
getPalettes =
  Task.perform
    PaletteLoadFail
    PaletteLoadSucceed
    (Http.get decodePalettes "/data/palettes.json")


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Init -> (model, getPalettes)
    PaletteLoadFail _ -> (model, Cmd.none)
    PaletteLoadSucceed lst -> (Model lst, Cmd.none)
    Frame dt -> (model, Cmd.none)

main = Html.App.program
  { init = init
  , view = view
  , subscriptions = subs
  , update = update
  }
