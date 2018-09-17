{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Cache
  ( Cache
  , Cached (..)
  , Cacheable
  , cacheLookup
  , cacheSet
  , cacheInsert
  , cacheKeys
  ) where

import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Ref
import Data.Map.Monoidal as Monoidal
import Prologue

-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache term address value = Cache { unCache :: Monoidal.Map (Configuration term address value) (Set (Cached address value)) }
  deriving (Eq, Lower, Monoid, Ord, Reducer (Configuration term address value, Cached address value), Semigroup)

data Cached address value = Cached
  { cachedValue :: ValueRef address
  , cachedHeap  :: Heap address address value
  }
  deriving (Eq, Ord, Show)


type Cacheable term address value = (Ord address, Ord term, Ord value)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: Cacheable term address value => Configuration term address value -> Cache term address value -> Maybe (Set (Cached address value))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: Cacheable term address value => Configuration term address value -> Set (Cached address value) -> Cache term address value -> Cache term address value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: Cacheable term address value => Configuration term address value -> Cached address value -> Cache term address value -> Cache term address value
cacheInsert = curry cons

-- | Return all 'Configuration's in the provided cache.
cacheKeys :: Cache term address value -> [Configuration term address value]
cacheKeys = Monoidal.keys . unCache

instance (Show term, Show address, Show value) => Show (Cache term address value) where
  showsPrec d = showsUnaryWith showsPrec "Cache" d . map (second toList) . Monoidal.pairs . unCache
