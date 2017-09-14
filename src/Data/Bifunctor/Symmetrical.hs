module Data.Bifunctor.Symmetrical where

import Data.Bifunctor

class Bifunctor s => Symmetrical s where
  mirror :: s a b -> s b a
