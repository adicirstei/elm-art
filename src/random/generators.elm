module Random.Generators exposing(..)
import Color exposing(Color)
import Random exposing(..)
import Random.Extra exposing(..)
import Types exposing(Config, Particle, NonEmptyList, nelLength, nelGet, Palette)


position : Float -> Generator (Float, Float)
position radius =
  map2 (\a r -> (r * (cos a), r * (sin a))) (float 0.0 (2.0*pi)) (float 0.0 radius)

foreground : NonEmptyList Color -> Generator Color
foreground fg =
  map (\i -> nelGet i fg) (int 0 ((nelLength fg)-1))


duration : Generator (Float, Float)
duration =
  let range = float 0.0 500.0
  in
    flatMap (\max -> map2 (,) (float max max) (float 0.0 max)) range

particle : Config -> Palette -> Generator Particle
particle cfg pl =
  map6 (\p r (d,t) v s c ->
    Particle p r d t v s c)
    (position cfg.startArea)
    (float 0.01 cfg.maxRadius)
    duration
    (map2 (,) (float -1.0 1.0) (float -1.0 1.0))
    (float 0.5 2.0)
    (foreground pl.fg)
