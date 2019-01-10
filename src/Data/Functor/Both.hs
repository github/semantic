{-# LANGUAGE DerivingVia #-}

module Data.Functor.Both
( Both (..)
, runBothWith
) where

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Monoid.Generic
import GHC.Generics

-- | A computation over both sides of a pair.
data Both a = Both a a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Generic1, Generic)
  deriving Semigroup via GenericSemigroup (Both a)
  deriving Monoid    via GenericMonoid (Both a)
  deriving (Eq1, Show1, Ord1) via Generically Both

-- | Apply a function to `Both` sides of a computation.
-- The eliminator/catamorphism over 'Both'.
runBothWith :: (a -> a -> b) -> Both a -> b
runBothWith f (Both a b) = f a b
