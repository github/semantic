module Data.Bifunctor.These where

import Data.Bifunctor

data These a b = This a | That b | These a b
  deriving (Eq, Show)

-- | Eliminate These by case analysis.
these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these f _ _ (This this) = f this
these _ f _ (That that) = f that
these _ _ f (These this that) = f this that

-- | Return Just the value in This, or the first value in These, if any.
maybeFirst :: These a b -> Maybe a
maybeFirst = these Just (const Nothing) ((Just .) . const)

-- | Return Just the value in That, or the second value in These, if any.
maybeSecond :: These a b -> Maybe b
maybeSecond = these (const Nothing) Just ((Just .) . flip const)

swap :: These a b -> These b a
swap = these That This (flip These)

-- Instances

instance Bifunctor These where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)
  bimap f g (These a b) = These (f a) (g b)
