module Types exposing(..)

import Http
import Color
import Time

import Collage

type NonEmptyList a
  = RootElement a
  | ListItem a (NonEmptyList a)

fromList : List a -> Maybe (NonEmptyList a)
fromList l =
  case l of
    [x] -> Just (RootElement x)
    hd::tl -> Maybe.map (ListItem hd) <| fromList tl
    _ -> Nothing

toList : NonEmptyList a -> List a
toList nel =
  case nel of
    RootElement r -> [r]
    ListItem x rest -> x :: (toList rest)

head : NonEmptyList a -> a
head nel =
  case nel of
    RootElement r -> r
    ListItem i _ -> i

nelGet : Int -> NonEmptyList a -> a
nelGet idx nel =
  case nel of
    RootElement e -> e
    ListItem x rest -> if idx == 0 then x else nelGet (idx-1) rest


nelLength : NonEmptyList a -> Int
nelLength nel =
  case nel of
    RootElement _ -> 1
    ListItem _ rest -> 1+(nelLength rest)


type alias Palette =
  { bg : Color.Color
  , fg : NonEmptyList Color.Color
  }

type Msg
  = Init
  | PaletteLoadFail Http.Error
  | PaletteLoadSucceed (List Palette)
  | Frame Time.Time
  | Random Int


type alias Particle =
  { position : (Float, Float)
  , radius : Float
  , duration : Float
  , time : Time.Time
  , velocity : (Float, Float)
  , speed : Float
  , color : Color.Color
  , prev : (Float,Float)
  }

type alias Config =
  { pointilism : Float
  , noiseScalar : (Float, Float)
  , startArea : Float
  , maxRadius : Float
  , lineStyle : Collage.LineCap
  , interval : Float
  , count : Int
  , steps : Int
  }
