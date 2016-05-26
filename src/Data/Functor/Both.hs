{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Functor.Both where

import Data.Align
import Data.Bifunctor
import Data.Bifunctor.These
import Data.Maybe
import Prologue hiding (zipWith, fst, snd)
import qualified Prologue

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
fst = Prologue.fst . runBoth

-- | Runs the right side of a `Both`.
snd :: Both a -> a
snd = Prologue.snd . runBoth

unzip :: [Both a] -> Both [a]
unzip = foldr pair (pure [])
  where pair (Both (a, b)) (Both (as, bs)) = Both (a : as, b : bs)

instance Applicative Both where
  pure a = Both (a, a)
  Both (f, g) <*> Both (a, b) = Both (f a, g b)

instance Monoid a => Monoid (Both a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b


instance TotalCrosswalk Both where
  tsequenceL d = runBothWith (alignWith (\ these -> fromMaybe <$> d <*> maybeBothOfThese these))
