module Data.Bifunctor.These where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

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


mergeThese :: (a -> a -> a) -> These a a -> a
mergeThese = these id id


-- Instances

instance Bifunctor These where
  bimap f g = these (This . f) (That . g) ((. g) . These . f)

instance Bifoldable These where
  bifoldMap f g = these f g ((. g) . mappend . f)

instance Bitraversable These where
  bitraverse f _ (This a) = This <$> f a
  bitraverse _ g (That b) = That <$> g b
  bitraverse f g (These a b) = These <$> f a <*> g b
