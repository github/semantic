module Data.Bifunctor.These where

import Data.Bifunctor
import Data.Bifoldable

data These a b = This a | That b | These a b
  deriving (Eq, Show)

-- | Eliminate These by case analysis.
these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these f _ _ (This this) = f this
these _ f _ (That that) = f that
these _ _ f (These this that) = f this that

-- | Return a pair of values given These and defaults for either side.
fromThese :: a -> b -> These a b -> (a, b)
fromThese a b = these (flip (,) b) ((,) a) (,)

-- | Return Just the value in This, or the first value in These, if any.
maybeFst :: These a b -> Maybe a
maybeFst = these Just (const Nothing) ((Just .) . const)

-- | Return Just the value in That, or the second value in These, if any.
maybeSnd :: These a b -> Maybe b
maybeSnd = these (const Nothing) Just ((Just .) . flip const)


-- Instances

instance Bifunctor These where
  bimap f g = these (This . f) (That . g) ((. g) . These . f)

instance Bifoldable These where
  bifoldMap f g = these f g ((. g) . mappend . f)
