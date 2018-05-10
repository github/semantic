{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Cache where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Map.Monoidal as Monoidal
import Data.Semilattice.Lower
import Prologue

-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache term location value = Cache { unCache :: Monoidal.Map (Configuration term (Cell location) location value) (Set (value, Heap (Cell location) location value)) }
  deriving (Lower)

type Cacheable term location value = (Ord (Cell location value), Ord location, Ord term, Ord value)

deriving instance (Eq   term, Eq   location, Eq   value, Eq   (Cell location value)) => Eq   (Cache term location value)
deriving instance (Ord  term, Ord  location, Ord  value, Ord  (Cell location value)) => Ord  (Cache term location value)
deriving instance (Show term, Show location, Show value, Show (Cell location value)) => Show (Cache term location value)

deriving instance Cacheable term location value => Semigroup (Cache term location value)
deriving instance Cacheable term location value => Monoid    (Cache term location value)
deriving instance (Cacheable term location value, cell ~ Cell location) => Reducer (Configuration term cell location value, (value, Heap cell location value)) (Cache term location value)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: Cacheable term location value => Configuration term (Cell location) location value -> Cache term location value -> Maybe (Set (value, Heap (Cell location) location value))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: Cacheable term location value => Configuration term (Cell location) location value -> Set (value, Heap (Cell location) location value) -> Cache term location value -> Cache term location value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: Cacheable term location value => Configuration term (Cell location) location value -> (value, Heap (Cell location) location value) -> Cache term location value -> Cache term location value
cacheInsert = curry cons
