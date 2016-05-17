module Lists exposing(..)

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
