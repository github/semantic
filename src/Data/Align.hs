module Data.Align where

import Data.Bifunctor.These
import Data.Functor.Identity

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

instance Align Maybe where
  nil = Nothing
  a `align` b | Just a <- a, Just b <- b = Just (These a b)
              | Just a <- a = Just (This a)
              | Just b <- b = Just (That b)
              | otherwise = Nothing


class Functor t => Crosswalk t where
  crosswalk :: Align f => (a -> f b) -> t a -> f (t b)
  crosswalk f = sequenceL . fmap f

  sequenceL :: Align f => t (f a) -> f (t a)
  sequenceL = crosswalk id

instance Crosswalk Identity where
  crosswalk f = fmap Identity . f . runIdentity
