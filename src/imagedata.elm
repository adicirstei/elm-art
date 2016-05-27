port module ImageData exposing(..)

import Types exposing(..)
import Array

port askImageData : Maybe String -> Cmd msg
port data : (Maybe (Array.Array Int) -> msg) -> Sub msg
