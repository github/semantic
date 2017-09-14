module Data.Bifunctor.Symmetrical where

import Data.Bifunctor
import Data.Tuple (swap)

class Bifunctor s => Symmetrical s where
  mirror :: s a b -> s b a


instance Symmetrical (,) where
  mirror = swap

instance Symmetrical Either where
  mirror = either Right Left
