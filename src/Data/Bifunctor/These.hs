module Data.Bifunctor.These where

import Data.Bifunctor

data These a b = This a | That b | These a b
  deriving (Eq, Show)


-- Instances

instance Bifunctor These where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)
  bimap f g (These a b) = These (f a) (g b)
