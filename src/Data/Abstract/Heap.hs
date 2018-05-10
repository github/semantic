{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Data.Abstract.Heap where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Heap location value = Heap { unHeap :: Monoidal.Map location (Cell location value) }
  deriving (Lower)

deriving instance (Eq location, Eq (Cell location value)) => Eq (Heap location value)
deriving instance (Ord location, Ord (Cell location value)) => Ord (Heap location value)
deriving instance (Show location, Show (Cell location value)) => Show (Heap location value)
deriving instance Foldable (Cell location) => Foldable (Heap location)
deriving instance Functor (Cell location) => Functor (Heap location)
deriving instance Traversable (Cell location) => Traversable (Heap location)
deriving instance (Ord location, Semigroup (Cell location value)) => Semigroup (Heap location value)
deriving instance (Ord location, Semigroup (Cell location value)) => Monoid (Heap location value)
deriving instance (Ord location, Reducer value (Cell location value)) => Reducer (location, value) (Heap location value)

-- | Look up the cell of values for an 'Address' in a 'Heap', if any.
heapLookup :: Ord location => Address location value -> Heap location value -> Maybe (Cell location value)
heapLookup (Address address) = Monoidal.lookup address . unHeap

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: (Ord location, Foldable (Cell location)) => Address location value -> Heap location value -> Maybe [value]
heapLookupAll address = fmap toList . heapLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
heapInsert :: (Ord location, Reducer value (Cell location value)) => Address location value -> value -> Heap location value -> Heap location value
heapInsert (Address address) value = flip snoc (address, value)

-- | Manually insert a cell into the heap at a given address.
heapInit :: Ord location => Address location value -> Cell location value -> Heap location value -> Heap location value
heapInit (Address address) cell (Heap h) = Heap (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap location value -> Int
heapSize = Monoidal.size . unHeap

-- | Restrict a 'Heap' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Ord location => Heap location value -> Live location value -> Heap location value
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> Address address `liveMember` roots) m)
