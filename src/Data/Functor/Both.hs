module Data.Functor.Both where

import Prelude hiding (zipWith)

-- | A computation over both sides of a pair.
newtype Both a = Both { runBoth :: (a, a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Given two operands returns a functor operating on `Both`. This is a curried synonym for Both.
both :: a -> a -> Both a
both = curry Both

zip :: Both [a] -> [Both a]
zip = zipWith both

zipWith :: (a -> a -> b) -> Both [a] -> [b]
zipWith f (Both ([], _)) = []
zipWith f (Both (_, [])) = []
zipWith f (Both (a : as, b : bs)) = f a b : zipWith f (both as bs)

unzip :: [Both a] -> Both [a]
unzip = foldr pair (pure [])
  where pair (Both (a, b)) (Both (as, bs)) = Both (a : as, b : bs)

instance Applicative Both where
  pure a = Both (a, a)
  Both (f, g) <*> Both (a, b) = Both (f a, g b)

instance Monoid a => Monoid (Both a) where
  mempty = pure mempty
  mappend a b = pure mappend <*> a <*> b
