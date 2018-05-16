{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Address.Precise where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.IntMap.Monoidal as Monoidal
import Data.Monoid (Last(..))
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


-- | A cell holding a single value. Writes will replace any prior value.
--
--   This is equivalent to 'Data.Monoid.Last', but with a 'Show' instance designed to minimize the amount of text we have to scroll past in ghci.
newtype Latest value = Latest { unLatest :: Last value }
  deriving (Eq, Foldable, Functor, Lower, Monoid, Semigroup, Ord, Traversable)

instance Reducer value (Latest value) where
  unit = Latest . unit . Just

instance Show value => Show (Latest value) where
  showsPrec d = showsPrec d . getLast . unLatest


newtype Heap value = Heap { unHeap :: Monoidal.IntMap (Latest value) }
  deriving (Eq, Foldable, Functor, Lower, Monoid, Ord, Semigroup, Traversable)

-- | Look up the cell of values for an 'Address' in a 'Heap', if any.
heapLookup :: Address Precise value -> Heap value -> Maybe (Latest value)
heapLookup (Address (Precise address)) = Monoidal.lookup address . unHeap

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: Address Precise value -> Heap value -> Maybe [value]
heapLookupAll address = fmap toList . heapLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
heapInsert :: Address Precise value -> value -> Heap value -> Heap value
heapInsert (Address address) value = flip snoc (address, value)

-- | Manually insert a cell into the heap at a given address.
heapInit :: Address Precise value -> Latest value -> Heap value -> Heap value
heapInit (Address (Precise address)) cell (Heap h) = Heap (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap value -> Int
heapSize = Monoidal.size . unHeap

-- | Restrict a 'Heap' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Heap value -> Live Precise value -> Heap value
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> Address (Precise address) `liveMember` roots) m)


instance Reducer (Precise, value) (Heap value) where
  unit = Heap . unit . first unPrecise
  cons (Precise key, a) (Heap heap) = Heap (cons (key, a) heap)
  snoc (Heap heap) (Precise key, a) = Heap (snoc heap (key, a))

instance Show value => Show (Heap value) where
  showsPrec d = showsUnaryWith showsPrec "Heap" d . Monoidal.pairs . unHeap
