module Arithmetics exposing(..)

lerp : Float -> Float -> Float -> Float
lerp x y a =
  x * (1.0 - a) + y * a

-- clamp : Float -> Float -> Float -> Float
-- clamp value min max =
--   if min < max then
--     if value < min then min else if value > max then max else value
--   else
--     if value < max then max else if value > min then min else value


v2add : (Float, Float) -> (Float,Float) -> (Float,Float)
v2add (ax,ay) (bx,by) = (ax+bx, ay+by)

v2norm : (Float, Float) -> (Float,Float)
v2norm (x,y) =
  let len = x*x + y*y
  in
    if len > 0.0 then (x*(1.0 /(sqrt len)), x*(1.0 /(sqrt len))) else (1.0,1.0)


v2scale : Float -> (Float, Float) -> (Float,Float)
v2scale s (bx,by) = (s*bx, s*by)
