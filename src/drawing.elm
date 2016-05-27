module Drawing exposing(..)

import Collage exposing(..)
import Element exposing(toHtml)
import Random
import Color
import Time
import Noise
import List
import Html exposing(Html)

import Types exposing(..)
import Arithmetics exposing(..)
import Palette
import Random.Generators as RG
import Random.List
import Array

maps : Array.Array String
maps =

  [ "architecture.jpg", "church2.jpg", "city2.jpg", "city5.jpg", "eye.jpg", "fractal1.jpg"
  , "fractal2.jpg", "geo1.jpg", "geo3.jpg", "geo4.jpg", "geo5.jpg", "map7.jpg", "nature1.jpg"
  , "pat1.jpg", "scifi.jpg", "sym3.jpg", "sym6.jpg" ]
  |> List.map (\f -> "/maps/" ++ f )
  |> Array.fromList



(width, height) = (700.0, 500.0)
noiseScalar = (0.00001, 0.0001)
heightValue = 0.8
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
    (count, s8) = Random.step (Random.int 50 800) s7
    (steps, s9) = Random.step (Random.int 100 800) s8
    (image, s10) = Random.step (Random.int 0 (Array.length maps)) s9
    scale = min width height
  in (Config pointilism (fns, sns) (startArea*scale/2.0) maxRadius lineStyle interval count steps (Array.get image maps), s10)

type alias Model =
  { palette : Palette
  , seed : Random.Seed
  , particles : List Particle
  , table : Noise.PermutationTable
  , time : Float
  , config : Config
  , lines : List Form
  , imageMap : Maybe (Array.Array Int)
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
    log = Debug.log "config" config
    (particles, s4) = Random.step (Random.list config.count (RG.particle config pl)) s3

  in Model pl s4 particles table 0.0 config [] Nothing

art model  =

  collage (round width) (round height)
    ((rect width height |> filled model.palette.bg) :: model.lines )

render : Model -> Html Msg
render = art >> toHtml


step : Model -> Model
step model =
  let
    dt = model.config.interval
    (bg, fg) = (model.palette.bg, model.palette.fg)

    pointilism = lerp 0.000001 0.5 model.config.pointilism
    validParticles =
      model.particles
      |> List.filter (\p -> p.time < p.duration)

    (newParticles,seed) = Random.step (Random.list (model.config.count - (List.length validParticles)) (RG.particle model.config model.palette)) model.seed

    stepParticle : Particle -> (Particle, Form)
    stepParticle p =
      let

        (x,y) = p.position
        (fx,fy) = (clamp 0.0 (width-1.0) x, clamp 0.0 (height-1.0) y)
        n = Noise.noise3d model.table (fx*ps) (fy*ps) (p.duration + model.time)
        r = heightValue * p.radius * (Noise.noise3d model.table (x*pointilism) (y*pointilism) (p.duration + model.time))
        lineStyle = { defaultLine | width = r*p.time/p.duration, cap = model.config.lineStyle, join = Smooth, color = p.color }

        angle = 2.0 * pi * n
        speed = p.speed + (lerp 0.0 2.0 ps)
        velo = p.velocity |> v2add (cos angle, sin angle) |> v2norm
        move = v2scale speed velo
        newPos = p.position |> v2add move
        seg = segment p.position newPos |> traced lineStyle
      in
        ({ p
        | velocity = velo
        , position = newPos
        , prev = (x,y)
        , time = p.time + dt
        }, seg)

    (parts, lines) = List.map stepParticle (validParticles ++ newParticles) |> List.unzip
  in
    { model
    | particles = parts
    , time = model.time + dt
    , seed = seed
    , lines = model.lines ++ lines
    }
