import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Json.Decode as Json
import Task

import Html.App
import Http


import Palette exposing(..)

type alias Model =
  { palettes : List Palette }

type Msg
  = Init
  | PaletteLoadFail Http.Error
  | PaletteLoadSucceed (List Palette)

view : Model -> Html Msg
view model =
  div []
  [ h1 [] [ text "Generative art with Elm" ]
  , div [] [ text (toString (List.length model.palettes))]
  , button [onClick Init] [text "init"]
  ]

init : (Model, Cmd Msg)
init = (Model [], getPalettes)

subs : Model -> Sub Msg
subs = \_ -> Sub.none

getPalettes : Cmd Msg
getPalettes =
  Task.perform
    PaletteLoadFail
    PaletteLoadSucceed
    (Http.get decodePalettes "http://www.colourlovers.com/api/palettes/top?format=json")




update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Init -> (model, getPalettes)
    PaletteLoadFail _ -> (model, Cmd.none)
    PaletteLoadSucceed lst -> (Model lst, Cmd.none)

main = Html.App.program
  { init = init
  , view = view
  , subscriptions = subs
  , update = update
  }
