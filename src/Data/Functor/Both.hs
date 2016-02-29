module Data.Functor.Both where

import Prelude hiding (zipWith)

-- | A computation over both sides of a pair.
newtype Both a = Both { runBoth :: (a, a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Given two operands returns a functor operating on `Both`. This is a curried synonym for Both.
both :: a -> a -> Both a
both = curry Both

-- | Runs the left side of a `Both`.
runLeft :: Both a -> a
runLeft = fst . runBoth

-- | Runs the right side of a `Both`.
runRight :: Both a -> a
runRight = snd . runBoth

zip :: Both [a] -> [Both a]
zip = zipWith both

-- | Zip two lists by applying a function, using the default values to extend
-- | the shorter list.
zipWithDefaults :: (a -> a -> b) -> Both a -> Both [a] -> [b]
zipWithDefaults f ds as = take (uncurry max $ runBoth (length <$> as)) $ zipWith f ((++) <$> as <*> (repeat <$> ds))

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
