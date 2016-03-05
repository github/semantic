module Data.Adjoined where

newtype Adjoined a = Adjoined { unAdjoined :: [a] }
  deriving (Eq, Foldable, Functor, Show, Traversable)

class Adjoining a where
  adjacent :: a -> a -> Bool
  separate :: a -> a -> Bool
  separate a = not . adjacent a
