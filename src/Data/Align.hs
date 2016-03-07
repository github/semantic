module Data.Align where

import Data.Bifunctor.These

class Functor f => Align f where
  nil :: f a
  align :: f a -> f b -> f (These a b)
  align = alignWith id
  alignWith :: (These a b -> c) -> f a -> f b -> f c
  alignWith f a b = f <$> align a b

instance Align [] where
  nil = []
  alignWith f as [] = f . This <$> as
  alignWith f [] bs = f . That <$> bs
  alignWith f (a : as) (b : bs) = f (These a b) : alignWith f as bs
