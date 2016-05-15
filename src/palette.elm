module Palette exposing(..)

import Color.Convert
import Json.Decode exposing(..)

import Color
import String
import Result

type alias Color = Color.Color

type alias Palette = List Color

decodeColor : Decoder Color
decodeColor = customDecoder string hexToColor

decodePalette : Decoder Palette
decodePalette = list decodeColor

decodePalettes : Decoder (List Palette)
decodePalettes =
  list (at ["colors"] decodePalette)

hexToColor : String -> Result.Result String Color
hexToColor hcolor =
  Color.Convert.hexToColor hcolor
  |> Result.fromMaybe "Invalid color string"
