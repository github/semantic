module Data.Align where

import Data.Bifunctor.These
import Data.Functor.Identity

-- | A functor which can be aligned, essentially the union of (potentially) asymmetrical values.
-- |
-- | For example, this allows a zip over lists which pads out the shorter side with a default value.
class Functor f => Align f where
  nil :: f a
  align :: f a -> f b -> f (These a b)
  align = alignWith id
  alignWith :: (These a b -> c) -> f a -> f b -> f c
  alignWith f a b = f <$> align a b

instance Align [] where
  nil = []
  align as bs | la < lb = (That <$> take (lb - la) bs) ++ zipWith These as (drop (lb - la) bs)
              | la > lb = (This <$> take (la - lb) as) ++ zipWith These (drop (la - lb) as) bs
              | otherwise = zipWith These as bs
              where (la, lb) = (length as, length bs)

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

instance Crosswalk Maybe where
  crosswalk f = maybe nil (fmap Just) . fmap f
