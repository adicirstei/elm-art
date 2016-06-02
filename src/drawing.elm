module Drawing exposing(..)

import Collage exposing(..)
import Text 
import Element exposing(toHtml)
import Random
import Color
import Noise
import List
import Html exposing(Html)
import Types exposing(..)
import Arithmetics exposing(..)
import Palette
import Random.Generators as RG
import Array

maps : Array.Array String
maps =

  [ "architecture.jpg", "church2.jpg", "city2.jpg", "city5.jpg", "eye.jpg", "fractal1.jpg"
  , "fractal2.jpg", "geo1.jpg", "geo3.jpg", "geo4.jpg", "geo5.jpg", "map7.jpg", "nature1.jpg"
  , "pat1.jpg", "scifi.jpg", "sym3.jpg", "sym6.jpg", "pattern_dots_black_white.png" ]
  |> List.map (\f -> "/maps/" ++ f )
  |> Array.fromList


luminosity : Int -> Int -> Int -> Float
luminosity r g b =
  (toFloat r) * 0.299 + (toFloat g) * 0.587 + (toFloat b) * 0.114


(width, height) = (800.0, 600.0)

noiseScalar : (Float, Float)
noiseScalar = (0.00001, 0.0001)



randomLineCap : Random.Generator LineCap
randomLineCap = Random.map (\b -> if b then Flat else Round) Random.bool

printString : String -> Color.Color -> Form
printString str c =
  Text.fromString str
  |> Text.color c
  |> Text.height 24.0
  |> text
  |> move (-width/2.0 + 100.0, height/2.0 - 30.0)

stats : Model -> List Form
stats model =
  let 
    progressLine = { defaultLine | width = 5, color = head model.palette.fg } 
    progressLenght = width * (toFloat model.step) / (toFloat model.config.steps)
    image = 
      case model.config.image of 
        Nothing -> []
        Just im -> [ printString im (head model.palette.fg)]
  in 
    [ segment (-width/2.0, height/2.0-4.0) ((-width/2.0 + progressLenght), height/2.0-4.0) |> traced progressLine ]
    ++ image
  




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
    (count, s8) = Random.step (Random.int 50 500) s7
    (steps, s9) = Random.step (Random.int 100 500) s8
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
  , step : Int
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

  in Model pl s4 particles table 0.0 config [] Nothing 0

art : Model -> Element.Element
art model  =

  collage (round width) (round height)
    ((rect width height |> filled model.palette.bg) :: model.lines ++ (stats model))


render : Model -> Html Msg
render = art >> toHtml


view : Model -> Html Msg
view m =

  toHtml <|
    case m.imageMap of
      Nothing -> collage 700 500 [ rect 700 500 |> filled Color.black ]
      Just im ->
        collage 700 500
          (imgFromMap im
            |> (\ arr -> let z = Debug.log "lumi" (Array.length arr) in arr)
            |> Array.indexedMap
                (\i l ->
                  let
                    line = {defaultLine | color = Color.rgb l l 100}
                    x = toFloat (i % 700)-350.0
                    y = 250.0-(toFloat (i // 700))
                  in segment (x, y) (x + 1.0, y)
                      |> traced line)
            |> Array.toList )

imgFromMap : Array.Array Int -> Array.Array Int
imgFromMap imap =
  Array.initialize 350000
    (\i ->
      let
        r = Maybe.withDefault 0 (Array.get (i*4) imap)
        g = Maybe.withDefault 0 (Array.get (i*4 + 1) imap)
        b = Maybe.withDefault 0 (Array.get (i*4 + 2) imap)
      in
        round (luminosity r g b)
    )


update : Model -> Model
update m = m



step : Model -> Model
step model =
  case model.imageMap of
    Nothing -> model
    _ ->
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

            (x,y) = {-Debug.log "(x,y)"-} p.position

            (fx,fy) = {-Debug.log "(fx,fy)"-} (clamp (-width/2.0) ((width/2.0)-1) x, clamp ((-height/2.0)+1) (height/2.0) y)
            --{--
            (i,j) = {-Debug.log "(i,j)"-} ( floor <| width / 2.0 + fx, floor <| height / 2.0 - fy)
            hIndex = (j * (round width) + i) * 4
            red = model.imageMap `Maybe.andThen` (Array.get hIndex)
            green = model.imageMap `Maybe.andThen` (Array.get (hIndex+1))
            blue = model.imageMap `Maybe.andThen` (Array.get (hIndex+2))

            heightValue = (Maybe.withDefault 0.0 (Maybe.map3 luminosity red green blue))/255.0
            {-
            zzz =
              case heightValue of
                0.0 -> toString (fx, fy, i,j)
                _ -> ""
            de = Debug.log "cols" zzz
            --}

            --heightValue = 0.9
            ps = lerp (fst noiseScalar) (snd noiseScalar) heightValue

            n = Noise.noise3d model.table (fx*ps) (fy*ps) (p.duration + model.time)
            r = (lerp 0.01 1.0 heightValue) * p.radius * (Noise.noise3d model.table (x*pointilism) (y*pointilism) (p.duration + model.time))
            lineStyle = { defaultLine | width = r*p.time/p.duration, cap = model.config.lineStyle, join = Smooth, color = p.color }


            angle = 2.0 * pi * n
            speed = p.speed + (lerp 0.0 2.0 (1.0-heightValue))
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
        , step = model.step + 1
        }
