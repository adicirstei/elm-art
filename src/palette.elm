module Palette exposing(..)

import Color.Convert
import Json.Decode exposing(..)

import Color
import String
import Result
import Lists as L

type alias Color = Color.Color

type alias Palette =
  { bg : Color
  , fg : L.NonEmptyList Color
  }

toList : Palette -> List Color
toList p =
  p.bg::(L.toList p.fg)

fromColorList : List Color -> Maybe Palette
fromColorList lst =
  case lst of
    bg::fg -> Maybe.map (Palette bg) <| L.fromList fg
    _ -> Nothing


decodeColor : Decoder Color
decodeColor = customDecoder string hexToColor

decodePalette : Decoder Palette
decodePalette = customDecoder (list decodeColor) (\lst -> lst |> fromColorList |> Result.fromMaybe "invalid color number" )


decodePalettes : Decoder (List Palette)
decodePalettes =
  list (at ["colors"] decodePalette)

hexToColor : String -> Result.Result String Color
hexToColor hcolor =
  Color.Convert.hexToColor hcolor
  |> Result.fromMaybe "Invalid color string"
