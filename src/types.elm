module Types exposing(..)

{-| Types used by the app

# Types
@docs NonEmptyList, Palette, Msg, Particle, Config

# List functions
@docs fromList, toList, head, nelGet, nelLength
-}


import Http
import Color
import Time

import Collage



{-| A list with at least one element -}
type NonEmptyList a
  = RootElement a
  | ListItem a (NonEmptyList a)


{-| Convert a `List a` to a `Maybe (NonEmptyList a)`
-}
fromList : List a -> Maybe (NonEmptyList a)
fromList l =
  case l of
    [x] -> Just (RootElement x)
    hd::tl -> Maybe.map (ListItem hd) <| fromList tl
    _ -> Nothing

{-| Converts a NonEmptyList to a List -}
toList : NonEmptyList a -> List a
toList nel =
  case nel of
    RootElement r -> [r]
    ListItem x rest -> x :: (toList rest)

{-| Retreive the head of a NonEmptyList -}
head : NonEmptyList a -> a
head nel =
  case nel of
    RootElement r -> r
    ListItem i _ -> i

{-| Retreive the element speciffied by the index -}
nelGet : Int -> NonEmptyList a -> a
nelGet idx nel =
  case nel of
    RootElement e -> e
    ListItem x rest -> if idx == 0 then x else nelGet (idx-1) rest

{-| Length of a NonEmptyList -}
nelLength : NonEmptyList a -> Int
nelLength nel =
  case nel of
    RootElement _ -> 1
    ListItem _ rest -> 1+(nelLength rest)

{-| A Color palette -}
type alias Palette =
  { bg : Color.Color
  , fg : NonEmptyList Color.Color
  }

{-| Application message type -}
type Msg
  = Init
  | PaletteLoadFail Http.Error
  | PaletteLoadSucceed (List Palette)
  | Frame Time.Time
  | Random Int


{-| Particle -}
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


{-| Configuration -}
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
