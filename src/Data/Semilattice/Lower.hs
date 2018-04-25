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
  --   If @s@ is 'Bounded', we require 'lower' and 'minBound' to agree:
  --
  --   > lower = minBound
  --
  --   If @s@ is a 'Join' semilattice, 'lower' must be the identity of '(\/)':
  --
  --   > lower \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'lower' must be at least as small as every terminating value:
  --
  --   > compare lower a /= GT
  lower :: s
  default lower :: Bounded s => s
  lower = minBound

instance Lower b => Lower (a -> b) where lower = const lower

instance Lower (Maybe a) where lower = Nothing
instance Lower [a] where lower = []

-- containers
instance Lower (IntMap a) where lower = IntMap.empty
instance Lower IntSet where lower = IntSet.empty
instance Lower (Map k a) where lower = Map.empty
instance Lower (Set a) where lower = Set.empty
