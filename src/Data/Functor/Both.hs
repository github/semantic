{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Functor.Both where

import Data.Align
import Data.Coalescent
import Data.Bifunctor
import Data.Bifunctor.These
import Data.Maybe
import Prelude hiding (zipWith, fst, snd)
import qualified Prelude

-- | A computation over both sides of a pair.
newtype Both a = Both { runBoth :: (a, a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Given two operands returns a functor operating on `Both`. This is a curried synonym for Both.
both :: a -> a -> Both a
both = curry Both

-- | Construct Both with These values & defaults.
bothOfThese :: Both a -> These a a -> Both a
bothOfThese a = these (`both` snd a) (both (fst a)) both

-- | Construct Both (Maybe) with These values, defaulting to Nothing.
maybeBothOfThese :: These a a -> Both (Maybe a)
maybeBothOfThese = bothOfThese (pure Nothing) . bimap Just Just

-- | Apply a function to `Both` sides of a computation.
runBothWith :: (a -> a -> b) -> Both a -> b
runBothWith f = uncurry f . runBoth

-- | Runs the left side of a `Both`.
fst :: Both a -> a
fst = Prelude.fst . runBoth

-- | Runs the right side of a `Both`.
snd :: Both a -> a
snd = Prelude.snd . runBoth

unzip :: [Both a] -> Both [a]
unzip = foldr pair (pure [])
  where pair (Both (a, b)) (Both (as, bs)) = Both (a : as, b : bs)

instance Applicative Both where
  pure a = Both (a, a)
  Both (f, g) <*> Both (a, b) = Both (f a, g b)

instance Monoid a => Monoid (Both a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b


-- | A wrapper around `Both (Maybe a)` to allow total handling of partial operations.
newtype BothMaybe a = BothMaybe { runBothMaybe :: Both (Maybe a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Lift Both values into BothMaybe via Just.
justBoth :: Both a -> BothMaybe a
justBoth = BothMaybe . fmap Just

-- | Recover Both from BothMaybe given a default value.
bothWithDefault :: a -> BothMaybe a -> Both a
bothWithDefault a = fmap (fromMaybe a) . runBothMaybe

instance Applicative BothMaybe where
  pure = BothMaybe . pure . Just
  BothMaybe (Both (f, g)) <*> BothMaybe (Both (a, b)) = BothMaybe (both (f <*> a) (g <*> b))

instance Crosswalk BothMaybe where
  crosswalk f (BothMaybe ab) = runBothWith (alignWith (BothMaybe . maybeBothOfThese)) (maybe nil f <$> ab)

instance Coalescent a => Coalescent (BothMaybe a) where
  coalesce as bs = sequenceA (coalesce <$> as <*> bs)
