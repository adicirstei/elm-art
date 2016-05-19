module Palette exposing(..)

import Color.Convert
import Json.Decode exposing(..)

import Color
import String
import Result
import Types exposing(..)


toColorList : Palette -> List Color.Color
toColorList p =
  p.bg::(toList p.fg)

fromColorList : List Color.Color -> Maybe Palette
fromColorList lst =
  case lst of
    bg::fg -> Maybe.map (Palette bg) <| fromList fg
    _ -> Nothing


decodeColor : Decoder Color.Color
decodeColor = customDecoder string hexToColor

decodePalette : Decoder Palette
decodePalette = customDecoder (list decodeColor) (\lst -> lst |> fromColorList |> Result.fromMaybe "invalid color number" )


decodePalettes : Decoder (List Palette)
decodePalettes =
  list (at ["colors"] decodePalette)

hexToColor : String -> Result.Result String Color.Color
hexToColor hcolor =
  Color.Convert.hexToColor hcolor
  |> Result.fromMaybe "Invalid color string"
