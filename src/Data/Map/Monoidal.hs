{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module defines a 'Map' type whose 'Monoid' and 'Reducer' instances merge values using the 'Semigroup' instance for the underlying type.
module Data.Map.Monoidal where

import qualified Data.Map as Map
import Prologue hiding (Map)

newtype Map key value = Map { unMap :: Map.Map key value }
  deriving (Eq, Ord, Show)

instance (Ord key, Semigroup value) => Semigroup (Map key value) where
  Map a <> Map b = Map (Map.unionWith (<>) a b)

instance (Ord key, Semigroup value) => Monoid (Map key value) where
  mempty = Map Map.empty
  mappend = (<>)
