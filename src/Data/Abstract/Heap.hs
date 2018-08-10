{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Heap
  ( Heap
  , heapLookup
  , heapLookupAll
  , heapInsert
  , heapInit
  , heapSize
  , heapRestrict
  ) where

import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Heap address value = Heap { unHeap :: Monoidal.Map address (Set value) }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup)

-- | Look up the cell of values for an 'Address' in a 'Heap', if any.
heapLookup :: Ord address => address -> Heap address value -> Maybe (Set value)
heapLookup address = Monoidal.lookup address . unHeap

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: Ord address => address -> Heap address value -> Maybe [value]
heapLookupAll address = fmap toList . heapLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
heapInsert :: (Ord address, Ord value) => address -> value -> Heap address value -> Heap address value
heapInsert address value = flip snoc (address, value)

-- | Manually insert a cell into the heap at a given address.
heapInit :: Ord address => address -> Set value -> Heap address value -> Heap address value
heapInit address cell (Heap h) = Heap (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap address value -> Int
heapSize = Monoidal.size . unHeap

-- | Restrict a 'Heap' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Ord address => Heap address value -> Live address -> Heap address value
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> address `liveMember` roots) m)


instance (Ord address, Ord value) => Reducer (address, value) (Heap address value) where
  unit = Heap . unit
  cons (addr, a) (Heap heap) = Heap (cons (addr, a) heap)
  snoc (Heap heap) (addr, a) = Heap (snoc heap (addr, a))

instance (Show address, Show value) => Show (Heap address value) where
  showsPrec d = showsUnaryWith showsPrec "Heap" d . Monoidal.pairs . unHeap
