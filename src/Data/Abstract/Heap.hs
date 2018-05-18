{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Heap where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Heap location cell value = Heap { unHeap :: Monoidal.Map location (cell value) }
  deriving (Eq, Foldable, Functor, Lower, Monoid, Ord, Semigroup, Traversable)

-- | Look up the cell of values for an 'Address' in a 'Heap', if any.
heapLookup :: Ord location => Address location value -> Heap location cell value -> Maybe (cell value)
heapLookup (Address address) = Monoidal.lookup address . unHeap

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: (Ord location, Foldable cell) => Address location value -> Heap location cell value -> Maybe [value]
heapLookupAll address = fmap toList . heapLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
heapInsert :: (Ord location, Reducer value (cell value)) => Address location value -> value -> Heap location cell value -> Heap location cell value
heapInsert address value = flip snoc (address, value)

-- | Manually insert a cell into the heap at a given address.
heapInit :: Ord location => Address location value -> cell value -> Heap location cell value -> Heap location cell value
heapInit (Address address) cell (Heap h) = Heap (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap location cell value -> Int
heapSize = Monoidal.size . unHeap

-- | Restrict a 'Heap' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Ord location => Heap location cell value -> Live location value -> Heap location cell value
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> Address address `liveMember` roots) m)


instance (Ord location, Reducer value (cell value)) => Reducer (Address location value, value) (Heap location cell value) where
  unit = Heap . unit . first unAddress
  cons (Address key, a) (Heap heap) = Heap (cons (key, a) heap)
  snoc (Heap heap) (Address key, a) = Heap (snoc heap (key, a))

instance (Show location, Show (cell value)) => Show (Heap location cell value) where
  showsPrec d = showsUnaryWith showsPrec "Heap" d . Monoidal.pairs . unHeap
