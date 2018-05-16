{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module defines an 'IntMap' type whose 'Monoid' and 'Reducer' instances merge values using the 'Semigroup' instance for the underlying type.
module Data.IntMap.Monoidal
( IntMap
, Key
, lookup
, singleton
, size
, insert
, filterWithKey
, pairs
, module Reducer
) where

import Data.Aeson (ToJSON)
import qualified Data.IntMap as IntMap
import Data.Semigroup.Reducer as Reducer
import Data.Semilattice.Lower
import Prelude hiding (lookup)
import Prologue hiding (IntMap)

newtype IntMap value = IntMap { unIntMap :: IntMap.IntMap value }
  deriving (Eq, Eq1, Foldable, Functor, Ord, Ord1, Show, Show1, ToJSON, Traversable)

type Key = IntMap.Key


singleton :: Key -> value -> IntMap value
singleton k v = IntMap (IntMap.singleton k v)


lookup :: Key -> IntMap value -> Maybe value
lookup key = IntMap.lookup key . unIntMap

size :: IntMap value -> Int
size = IntMap.size . unIntMap

insert :: Key -> value -> IntMap value -> IntMap value
insert key value = IntMap . IntMap.insert key value . unIntMap

filterWithKey :: (Key -> value -> Bool) -> IntMap value -> IntMap value
filterWithKey f = IntMap . IntMap.filterWithKey f . unIntMap


pairs :: IntMap value -> [(Key, value)]
pairs = IntMap.toList . unIntMap


instance Semigroup value => Semigroup (IntMap value) where
  IntMap a <> IntMap b = IntMap (IntMap.unionWith (<>) a b)

instance Semigroup value => Monoid (IntMap value) where
  mempty = IntMap IntMap.empty
  mappend = (<>)

instance Reducer a value => Reducer (Int, a) (IntMap value) where
  unit (key, a) = IntMap (IntMap.singleton key (unit a))
  cons (key, a) (IntMap m) = IntMap (IntMap.insertWith (<>) key (unit a) m)
  snoc (IntMap m) (key, a) = IntMap (IntMap.insertWith (flip (<>)) key (unit a) m)

instance Lower (IntMap value) where lowerBound = IntMap lowerBound
