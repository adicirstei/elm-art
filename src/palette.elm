module Palette exposing(..)

import ParseInt exposing (parseIntHex)
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
  let
    rc = Result.map3 Color.rgb (parseIntHex (String.slice 0 2 hcolor)) (parseIntHex (String.slice 2 4 hcolor)) (parseIntHex (String.slice 4 6 hcolor))
  in
    case rc of
      Err _ -> Err "Invalid color string"
      Ok v -> Ok v
