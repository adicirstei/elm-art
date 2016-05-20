module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)
import Random
import Color
import Time
import Noise
import Array

import Types exposing(..)
import Arithmetics exposing(..)
import Palette

randomLineCap : Random.Generator LineCap
randomLineCap = Random.map (\b -> if b then Flat else Round) Random.bool


createConfig : Random.Seed -> (Config, Random.Seed)
createConfig seed =
  let
    (pointilism, s1) = Random.step (Random.float 0.0 0.1) seed
    (fns, s2) = Random.step (Random.float 0.000001 0.000001) s1
    (sns, s3) = Random.step (Random.float 0.0002 0.004) s2
    (startArea, s4) = Random.step (Random.float 0.0 1.5) s3
    (maxRadius, s5) = Random.step (Random.float 5.0 100.0) s4
    (lineStyle, s6) = Random.step randomLineCap s5
    (interval, s7) = Random.step (Random.float 0.001 0.01) s6
    (count, s8) = Random.step (Random.int 50 2000) s7
    (steps, s9) = Random.step (Random.int 100 1000) s8
  in (Config pointilism (fns, sns) startArea maxRadius lineStyle interval count steps, s9)

type alias Model =
  { palette : Palette
  , seed : Random.Seed
  , particles : Array.Array Particle
  , table : Noise.PermutationTable
  , config : Config
  }

createParticles : Config -> Random.Seed -> (Array Particle, Random.Seed)
particle cfg seed =

  Particle 33.0

newDrawingModel : Int -> List Palette -> Model
newDrawingModel s lp =
  let
    seed = Random.initialSeed s
    (idx, s1) = Random.step (Random.int 0 (List.length lp)) seed
    pl = List.drop idx lp
        |> List.head
        |> Maybe.withDefault Palette.defaultPalette
    (table, s2) = Noise.permutationTable s1
    (config, s3) = createConfig s2
  in Model pl s3 (Array.initialize config.count particle) table config

art model  =
  let
    (bg, fg) = (model.palette.bg, model.palette.fg)
    --x = Debug.log "Drawing.model" model
    myLine = { defaultLine | width = 4.5, cap = Round, join = Smooth, color = head fg }
    pointilism = lerp 0.000001 0.5 model.config.pointilism
  in
    collage 900 600
      (rect 900 600 |> filled bg)
      :: model.particles |> List.map (a -> b) List a

render : Model -> Html Msg
render = art >> toHtml

stepParticle : Time.Time -> Particle -> Particle
stepParticle dt p = p


step : Time.Time -> Model -> Model
step dt model =
  {model| particles = model.particles}
