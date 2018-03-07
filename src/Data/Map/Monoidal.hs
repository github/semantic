-- | This module defines a 'Map' type whose 'Monoid' and 'Reducer' instances merge values using the 'Semigroup' instance for the underlying type.
module Data.Map.Monoidal where

import qualified Data.Map as Map

newtype Map key value = Map { unMap :: Map.Map key value }
