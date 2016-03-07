module Data.Functor.Both where

import Data.Adjoined
import Data.Bifunctor
import Prelude hiding (zipWith, fst, snd)
import qualified Prelude

-- | A computation over both sides of a pair.
newtype Both a = Both { runBoth :: (a, a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Given two operands returns a functor operating on `Both`. This is a curried synonym for Both.
both :: a -> a -> Both a
both = curry Both

-- | Apply a function to `Both` sides of a computation.
runBothWith :: (a -> a -> b) -> Both a -> b
runBothWith f = uncurry f . runBoth

-- | Runs the left side of a `Both`.
fst :: Both a -> a
fst = Prelude.fst . runBoth

-- | Runs the right side of a `Both`.
snd :: Both a -> a
snd = Prelude.snd . runBoth

zip :: Both [a] -> [Both a]
zip = zipWith both

-- | Split a `Both` of pairs into a pair of `Both`s.
transpose :: Both (a, b) -> (Both a, Both b)
transpose = runBothWith (uncurry bimap . bimap both both)

-- | Zip two lists by applying a function, using the default values to extend
-- | the shorter list.
zipWithDefaults :: (a -> a -> b) -> Both a -> Both [a] -> [b]
zipWithDefaults f ds as = take (runBothWith max (length <$> as)) $ zipWith f ((++) <$> as <*> (repeat <$> ds))

zipWith :: (a -> a -> b) -> Both [a] -> [b]
zipWith _ (Both ([], _)) = []
zipWith _ (Both (_, [])) = []
zipWith f (Both (a : as, b : bs)) = f a b : zipWith f (both as bs)

unzip :: [Both a] -> Both [a]
unzip = foldr pair (pure [])
  where pair (Both (a, b)) (Both (as, bs)) = Both (a : as, b : bs)

instance Applicative Both where
  pure a = Both (a, a)
  Both (f, g) <*> Both (a, b) = Both (f a, g b)

instance Monoid a => Monoid (Both a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

instance (PartialSemigroup a, Monoid a) => PartialSemigroup (Both a) where
  coalesce = coalesceBy coalesce

coalesceBy :: Monoid a => (a -> a -> Maybe a) -> Both a -> Both a -> Maybe (Both a)
coalesceBy coalesce a b = case coalesce <$> a <*> b of
  Both (Just l, Just r) -> Just (both l r)
  Both (Nothing, Just r) -> Just (both (fst a `mappend` fst b) r)
  Both (Just l, Nothing) -> Just (both l (snd a `mappend` snd b))
  Both (Nothing, Nothing) -> Nothing
