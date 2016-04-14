module Data.Bifunctor.These where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Arrow

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

-- | Given a pair of Maybes, produce a These containing Just their values, or Nothing if they haven’t any.
maybeThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeThese (Just a) (Just b) = Just (These a b)
maybeThese (Just a) _ = Just (This a)
maybeThese _ (Just b) = Just (That b)
maybeThese _ _ = Nothing

-- | Return Just the value in This, or the first value in These, if any.
maybeFst :: These a b -> Maybe a
maybeFst = these Just (const Nothing) ((Just .) . const)

-- | Return Just the value in That, or the second value in These, if any.
maybeSnd :: These a b -> Maybe b
maybeSnd = these (const Nothing) Just ((Just .) . flip const)

-- | Like `<*>`, but it returns its result in `Maybe` since the result is the intersection of the shapes of the inputs.
apThese :: These (a -> b) (c -> d) -> These a c -> Maybe (These b d)
apThese fg ab = uncurry maybeThese $ uncurry (***) (bimap (<*>) (<*>) (unpack fg)) (unpack ab)
  where unpack = fromThese Nothing Nothing . bimap Just Just


newtype Union a b = Union { getUnion :: Maybe (These a b) }
  deriving (Eq, Show)

newtype Intersection a b = Intersection { getIntersection :: Maybe (These a b) }
  deriving (Eq, Show)


-- Instances

instance Bifunctor These where
  bimap f g = these (This . f) (That . g) ((. g) . These . f)

instance Bifoldable These where
  bifoldMap f g = these f g ((. g) . mappend . f)

instance Bitraversable These where
  bitraverse f _ (This a) = This <$> f a
  bitraverse _ g (That b) = That <$> g b
  bitraverse f g (These a b) = These <$> f a <*> g b

instance (Monoid a, Monoid b) => Monoid (Union a b) where
  mempty = Union Nothing
  Union (Just a) `mappend` Union (Just b) = Union $ uncurry maybeThese $ uncurry (***) (bimap mappend mappend (unpack a)) (unpack b)
    where unpack = fromThese Nothing Nothing . bimap Just Just
  Union (Just a) `mappend` _ = Union $ Just a
  Union _ `mappend` Union (Just b) = Union $ Just b
  _ `mappend` _ = Union Nothing

instance (Monoid a, Monoid b) => Monoid (Intersection a b) where
  mempty = Intersection Nothing
  Intersection a `mappend` Intersection b = Intersection $ maybe a (maybe Just (apThese . bimap mappend mappend) a) b
