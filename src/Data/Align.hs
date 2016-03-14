module Data.Align where

import Data.Bifunctor.These
import Data.Functor.Identity

-- | A functor which can be aligned, essentially the union of (potentially) asymmetrical values.
-- |
-- | For example, this allows a zip over lists which pads out the shorter side with a default value.
class Functor f => Align f where
  -- | The empty value. The identity value for `align` (modulo the `This` or `That` constructor wrapping the results).
  nil :: f a
  -- | Combine two structures into a structure of `These` holding pairs of values in `These` where they overlap, and individual values in `This` and `That` elsewhere.
  -- |
  -- | Analogous with `zip`.
  align :: f a -> f b -> f (These a b)
  align = alignWith id
  -- | Combine two structures into a structure by applying a function to pairs of values in `These` where they overlap, and individual values in `This` and `That` elsewhere.
  -- |
  -- | Analogous with `zipWith`.
  alignWith :: (These a b -> c) -> f a -> f b -> f c
  alignWith f a b = f <$> align a b

instance Align [] where
  nil = []
           -- | The second list is longer, so map its prefix into `That` and zip the rest.
  align as bs | la < lb, (prefix, overlap) <- splitAt (lb - la) bs = (That <$> prefix) ++ zipWith These as overlap
           -- | The first list is longer, so map its prefix into `This` and zip the rest.
              | la > lb, (prefix, overlap) <- splitAt (la - lb) as = (This <$> prefix) ++ zipWith These overlap bs
           -- | They’re of equal length, so zip into `These`.
              | otherwise = zipWith These as bs
              where (la, lb) = (length as, length bs)

instance Align Maybe where
  nil = Nothing
  a `align` b | Just a <- a, Just b <- b = Just (These a b)
              | Just a <- a = Just (This a)
              | Just b <- b = Just (That b)
              | otherwise = Nothing


-- | A functor which can be traversed through an `Align`able functor, inverting the nesting of one in the other.
-- |
-- | Analogous with `zip`, in that it can e.g. turn a tuple of lists into a list of tuples.
class Functor t => Crosswalk t where
  -- | Embed a structure into an `Align`able functor by mapping its elements into that functor and convoluting (inverting the embedding).
  crosswalk :: Align f => (a -> f b) -> t a -> f (t b)
  crosswalk f = sequenceL . fmap f

  -- | Convolute (invert the embedding of) a structure over an `Align`able functor.
  sequenceL :: Align f => t (f a) -> f (t a)
  sequenceL = crosswalk id

instance Crosswalk Identity where
  crosswalk f = fmap Identity . f . runIdentity

instance Crosswalk Maybe where
  crosswalk f = maybe nil (fmap Just) . fmap f

class Functor t => TotalCrosswalk t where
  tcrosswalk :: Align f => t b -> (a -> f b) -> t a -> f (t b)
  tcrosswalk d f = tsequenceL d . fmap f

  tsequenceL :: Align f => t a -> t (f a) -> f (t a)
  tsequenceL d = tcrosswalk d id
