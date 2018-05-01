{-# LANGUAGE DefaultSignatures #-}
module Data.Semilattice.Lower
( Lower (..)
) where

import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Set as Set

class Lower s where
  -- | The greatest lower bound of @s@.
  --
  --   Laws:
  --
  --   If @s@ is 'Bounded', we require 'lowerBound' and 'minBound' to agree:
  --
  --   > lowerBound = minBound
  --
  --   If @s@ is a 'Join' semilattice, 'lowerBound' must be the identity of '(\/)':
  --
  --   > lowerBound \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'lowerBound' must be at least as small as every terminating value:
  --
  --   > compare lowerBound a /= GT
  lowerBound :: s
  default lowerBound :: Bounded s => s
  lowerBound = minBound

instance Lower b => Lower (a -> b) where lowerBound = const lowerBound

instance Lower (Maybe a) where lowerBound = Nothing
instance Lower [a] where lowerBound = []

-- containers
instance Lower (IntMap a) where lowerBound = IntMap.empty
instance Lower IntSet where lowerBound = IntSet.empty
instance Lower (Map k a) where lowerBound = Map.empty
instance Lower (Set a) where lowerBound = Set.empty
