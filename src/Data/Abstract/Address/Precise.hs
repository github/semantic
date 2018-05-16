{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Address.Precise where

import Data.Abstract.Address
import Data.Abstract.Environment
import qualified Data.IntMap.Monoidal as Monoidal
import qualified Data.IntSet as IntSet
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
heapInsert address value = flip snoc (address, value)

-- | Manually insert a cell into the heap at a given address.
heapInit :: Address Precise value -> Latest value -> Heap value -> Heap value
heapInit (Address (Precise address)) cell (Heap h) = Heap (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap value -> Int
heapSize = Monoidal.size . unHeap

-- | Restrict a 'Heap' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Heap value -> Live value -> Heap value
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> Address (Precise address) `liveMember` roots) m)


instance Reducer (Address Precise value, value) (Heap value) where
  unit = Heap . unit . first (unPrecise . unAddress)
  cons (Address (Precise key), a) (Heap heap) = Heap (cons (key, a) heap)
  snoc (Heap heap) (Address (Precise key), a) = Heap (snoc heap (key, a))

instance Show value => Show (Heap value) where
  showsPrec d = showsUnaryWith showsPrec "Heap" d . map (first Precise) . Monoidal.pairs . unHeap


-- | A set of live addresses (whether roots or reachable).
newtype Live value = Live { unLive :: IntSet }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

fromAddresses :: Foldable t => t (Address Precise value) -> Live value
fromAddresses = Prologue.foldr liveInsert lowerBound

-- | Construct a 'Live' set containing only the given address.
liveSingleton :: Address Precise value -> Live value
liveSingleton = Live . IntSet.singleton . unPrecise . unAddress

-- | Insert an address into a 'Live' set.
liveInsert :: Address Precise value -> Live value -> Live value
liveInsert (Address (Precise addr)) = Live . IntSet.insert addr . unLive

-- | Delete an address from a 'Live' set, if present.
liveDelete :: Address Precise value -> Live value -> Live value
liveDelete (Address (Precise addr)) = Live . IntSet.delete addr . unLive

-- | Compute the (asymmetric) difference of two 'Live' sets, i.e. delete every element of the second set from the first set.
liveDifference :: Live value -> Live value -> Live value
liveDifference = fmap Live . (IntSet.difference `on` unLive)

-- | Test whether an 'Address' is in a 'Live' set.
liveMember :: Address Precise value -> Live value -> Bool
liveMember (Address (Precise addr)) = IntSet.member addr . unLive

-- | Decompose a 'Live' set into a pair of one member address and the remaining set, or 'Nothing' if empty.
liveSplit :: Live value -> Maybe (Address Precise value, Live value)
liveSplit = fmap (bimap (Address . Precise) Live) . IntSet.minView . unLive


instance Show (Live value) where
  showsPrec d = showsUnaryWith showsPrec "Live" d . map Precise . IntSet.toList . unLive


-- | A single point in a program’s execution.
data Configuration term value = Configuration
  { configurationTerm        :: term                      -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots       :: Live value                -- ^ The set of rooted addresses.
  , configurationEnvironment :: Environment Precise value -- ^ The environment binding any free variables in 'configurationTerm'.
  , configurationHeap        :: Heap value                -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)
