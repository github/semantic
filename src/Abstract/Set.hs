{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Abstract.Set where

import Data.Bifunctor (second)
import Data.Function (on)
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set

newtype Set a = Set { unSet :: Set.Set a }
  deriving (Eq, Eq1, Foldable, Monoid, Ord, Ord1, Pointed, Semigroup, Show, Show1)

member :: Ord a => a -> Set a -> Bool
member = (. unSet) . Set.member

insert :: Ord a => a -> Set a -> Set a
insert a = Set . Set.insert a . unSet

delete :: Ord a => a -> Set a -> Set a
delete a = Set . Set.delete a . unSet

split :: Ord a => Set a -> Maybe (a, Set a)
split = fmap (second Set) . Set.minView . unSet

difference :: Ord a => Set a -> Set a -> Set a
difference = (Set .) . (Set.difference `on` unSet)
