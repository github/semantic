{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module defines a 'Map' type whose 'Monoid' and 'Reducer' instances merge values using the 'Semigroup' instance for the underlying type.
module Data.Map.Monoidal
( Map
, lookup
, singleton
, size
, insert
, filterWithKey
, pairs
, module Reducer
) where

import Data.Aeson (ToJSON)
import qualified Data.Map as Map
import Data.Semigroup.Reducer as Reducer
import Prelude hiding (lookup)
import Prologue hiding (Map)

newtype Map key value = Map { unMap :: Map.Map key value }
  deriving (Eq, Eq1, Eq2, Foldable, Functor, Ord, Ord1, Ord2, Show, Show1, Show2, ToJSON, Traversable)


singleton :: key -> value -> Map key value
singleton k v = Map (Map.singleton k v)


lookup :: Ord key => key -> Map key value -> Maybe value
lookup key = Map.lookup key . unMap

size :: Map key value -> Int
size = Map.size . unMap

insert :: Ord key => key -> value -> Map key value -> Map key value
insert key value = Map . Map.insert key value . unMap

filterWithKey :: (key -> value -> Bool) -> Map key value -> Map key value
filterWithKey f = Map . Map.filterWithKey f . unMap


pairs :: Map key value -> [(key, value)]
pairs = Map.toList . unMap


instance (Ord key, Semigroup value) => Semigroup (Map key value) where
  Map a <> Map b = Map (Map.unionWith (<>) a b)

instance (Ord key, Semigroup value) => Monoid (Map key value) where
  mempty = Map Map.empty
  mappend = (<>)

instance (Ord key, Reducer a value) => Reducer (key, a) (Map key value) where
  unit (key, a) = Map (Map.singleton key (unit a))
  cons (key, a) (Map m) = Map (Map.insertWith (<>) key (unit a) m)
  snoc (Map m) (key, a) = Map (Map.insertWith (flip (<>)) key (unit a) m)

instance Lower (Map key value) where lowerBound = Map lowerBound
