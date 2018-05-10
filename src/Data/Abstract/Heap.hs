{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Data.Abstract.Heap where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Heap cell location value = Heap { unHeap :: Monoidal.Map location (cell value) }
  deriving (Eq, Foldable, Functor, Lower, Monoid, Ord, Semigroup, Show, Traversable)

deriving instance (Ord location, Reducer value (cell value)) => Reducer (location, value) (Heap cell location value)

-- | Look up the cell of values for an 'Address' in a 'Heap', if any.
heapLookup :: Ord location => Address location value -> Heap cell location value -> Maybe (cell value)
heapLookup (Address address) = Monoidal.lookup address . unHeap

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: (Ord location, Foldable cell) => Address location value -> Heap cell location value -> Maybe [value]
heapLookupAll address = fmap toList . heapLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
heapInsert :: (Ord location, Reducer value (cell value)) => Address location value -> value -> Heap cell location value -> Heap cell location value
heapInsert (Address address) value = flip snoc (address, value)

-- | Manually insert a cell into the heap at a given address.
heapInit :: Ord location => Address location value -> cell value -> Heap cell location value -> Heap cell location value
heapInit (Address address) cell (Heap h) = Heap (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap cell location value -> Int
heapSize = Monoidal.size . unHeap

-- | Restrict a 'Heap' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Ord location => Heap cell location value -> Live location value -> Heap cell location value
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> Address address `liveMember` roots) m)
