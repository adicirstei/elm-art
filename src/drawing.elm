module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)
import Random
import Color
import Time
import Noise
import Array
import Html exposing(Html)

import Types exposing(..)
import Arithmetics exposing(..)
import Palette
import Random.Generators as RG
import Random.Array


(width, height) = (800.0, 600.0)
noiseScalar = (0.00001, 0.0001)
heightValue = 0.5
ps = lerp (fst noiseScalar) (snd noiseScalar) heightValue

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
  , time : Float
  , config : Config
  }


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
    (particles, s4) = Random.step (Random.Array.array config.count (RG.particle config pl)) s3

  in Model pl s4 particles table 0.0 config

art model  =
  let
    (bg, fg) = (model.palette.bg, model.palette.fg)

    pointilism = lerp 0.000001 0.5 model.config.pointilism
    renderParticle : Particle -> Form
    renderParticle p =
      let
        --debug = Debug.log "p.time/p.duration" (if p.time>p.duration then (p.time,p.duration) else (0.0,0.0) )
        (x,y) = p.prev
        r = heightValue * p.radius * (Noise.noise3d model.table (x*pointilism) (y*pointilism) (p.duration + model.time))
        lineStyle = { defaultLine | width = r*p.time/p.duration, cap = model.config.lineStyle, join = Smooth, color = p.color }

      in
        segment p.prev p.position
        |> traced lineStyle
  in
    collage (round width) (round height)
      ((rect width height |> filled bg) :: (model.particles |> Array.map renderParticle |> Array.toList ) )

render : Model -> Html Msg
render = art >> toHtml


step : Time.Time -> Model -> Model
step dt model =
  let

    pointilism = lerp 0.000001 0.5 model.config.pointilism
    validParticles = model.particles |> Array.filter (\p -> p.time <= p.duration)
    (newParticles,seed) = Random.step (Random.Array.array (model.config.count - (Array.length validParticles)) (RG.particle model.config model.palette)) model.seed

    stepParticle : Particle -> Particle
    stepParticle p =
      let
        (x,y) = p.position
        (fx,fy) = (clamp 0.0 (width-1.0) x, clamp 0.0 (height-1.0) y)
        n = Noise.noise3d model.table (fx*ps) (fy*ps) (p.duration + model.time)
        angle = 2.0 * pi * n
        speed = p.speed + (lerp 0.0 2.0 ps)
        velo = p.velocity |> v2add (cos angle, sin angle) |> v2norm
        move = v2scale speed velo

      in
        { p
        | velocity = velo
        , position = p.position |> v2add move
        , prev = (x,y)
        , time = p.time + dt/8.0
        }
  in
    { model
    | particles = Array.map stepParticle (Array.append validParticles newParticles)
    , time = model.time + dt
    , seed = seed
    }
