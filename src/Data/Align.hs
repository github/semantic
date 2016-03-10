module Data.Align where

import Data.Bifunctor.These
import Data.Functor.Identity

class Functor f => Align f where
  nil :: f a
  align :: f a -> f b -> f (These a b)
  align = alignWith id
  alignWith :: (These a b -> c) -> f a -> f b -> f c

instance Align [] where
  nil = []
  alignWith f as [] = f . This <$> as
  alignWith f [] bs = f . That <$> bs
  alignWith f (a : as) (b : bs) = f (These a b) : alignWith f as bs


class Crosswalk t where
  sequenceL :: Align f => t (f a) -> f (t a)

instance Crosswalk Identity where
  sequenceL = fmap Identity . runIdentity
