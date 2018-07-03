{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Cache where

import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Ref
import Data.Map.Monoidal as Monoidal
import Prologue

-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache term address cell value = Cache { unCache :: Monoidal.Map (Configuration term address cell value) (Set (Cached address cell value)) }
  deriving (Eq, Lower, Monoid, Ord, Reducer (Configuration term address cell value, Cached address cell value), Semigroup)

data Cached address cell value = Cached
  { cachedValue :: ValueRef address
  , cachedHeap  :: Heap address cell value
  }
  deriving (Eq, Ord, Show)


type Cacheable term address cell value = (Ord (cell value), Ord address, Ord term, Ord value)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: Cacheable term address cell value => Configuration term address cell value -> Cache term address cell value -> Maybe (Set (Cached address cell value))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: Cacheable term address cell value => Configuration term address cell value -> Set (Cached address cell value) -> Cache term address cell value -> Cache term address cell value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: Cacheable term address cell value => Configuration term address cell value -> Cached address cell value -> Cache term address cell value -> Cache term address cell value
cacheInsert = curry cons

-- | Return all 'Configuration's in the provided cache.
cacheKeys :: Cache term address cell value -> [Configuration term address cell value]
cacheKeys = Monoidal.keys . unCache

instance (Show term, Show address, Show (cell value), Show value) => Show (Cache term address cell value) where
  showsPrec d = showsUnaryWith showsPrec "Cache" d . map (second toList) . Monoidal.pairs . unCache
