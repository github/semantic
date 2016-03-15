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


-- | A functor which can be traversed through an `Align`able functor, inverting the nesting of one in the other, given some default value.
-- |
-- | Analogous with `zip`, in that it can e.g. turn a tuple of lists into a list of tuples.
class Functor t => TotalCrosswalk t where
  -- | Given some default value, embed a structure into an `Align`able functor by mapping its elements into that functor and convoluting (inverting the embedding).
  tcrosswalk :: Align f => t b -> (a -> f b) -> t a -> f (t b)
  tcrosswalk d f = tsequenceL d . fmap f

  -- | Given some default value, convolute (invert the embedding of) a structure over an `Align`able functor.
  tsequenceL :: Align f => t a -> t (f a) -> f (t a)
  tsequenceL d = tcrosswalk d id

instance TotalCrosswalk Identity where
  tcrosswalk _ f = fmap Identity . f . runIdentity
