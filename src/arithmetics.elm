module Arithmetics exposing(..)

lerp : Float -> Float -> Float -> Float
lerp x y a =
  x * (1.0 - a) + y * a

clamp : Float -> Float -> Float -> Float
clamp min max value =
  if min < max then
    if value < min then min else if value > max then max else value
  else
    if value < max then max else if value > min then min else value
