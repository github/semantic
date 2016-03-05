module Data.Adjoined where

newtype Adjoined a = Adjoined { unAdjoined :: [a] }
  deriving (Eq, Foldable, Functor, Show, Traversable)

class PartialSemigroup a where
  coalesce :: a -> a -> Maybe a
