module Data.Bifunctor.These where

data These a b = This a | That b | These a b
  deriving (Eq, Show)
