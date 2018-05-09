{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Cache where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Map.Monoidal as Monoidal
import Data.Semilattice.Lower
import Prologue

-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache term location value = Cache { unCache :: Monoidal.Map (Configuration term location value) (Set (value, Heap location value)) }
  deriving (Lower)

type Cacheable term location value = (Ord (Cell location value), Ord location, Ord term, Ord value)

deriving instance (Eq   term, Eq   location, Eq   value, Eq   (Cell location value)) => Eq   (Cache term location value)
deriving instance (Ord  term, Ord  location, Ord  value, Ord  (Cell location value)) => Ord  (Cache term location value)
deriving instance (Show term, Show location, Show value, Show (Cell location value)) => Show (Cache term location value)

deriving instance Cacheable term location value => Semigroup (Cache term location value)
deriving instance Cacheable term location value => Monoid    (Cache term location value)
deriving instance Cacheable term location value => Reducer (Configuration term location value, (value, Heap location value)) (Cache term location value)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: Cacheable term location value => Configuration term location value -> Cache term location value -> Maybe (Set (value, Heap location value))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: Cacheable term location value => Configuration term location value -> Set (value, Heap location value) -> Cache term location value -> Cache term location value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: Cacheable term location value => Configuration term location value -> (value, Heap location value) -> Cache term location value -> Cache term location value
cacheInsert = curry cons


instance (Eq   term, Eq   location, Eq1   (Cell location)) => Eq1   (Cache term location) where
  liftEq eqV (Cache c1) (Cache c2) = liftEq2 (liftEq eqV) (liftEq (liftEq2 eqV (liftEq eqV))) c1 c2

instance (Ord  term, Ord  location, Ord1  (Cell location)) => Ord1  (Cache term location) where
  liftCompare compareV (Cache c1) (Cache c2) = liftCompare2 (liftCompare compareV) (liftCompare (liftCompare2 compareV (liftCompare compareV))) c1 c2

instance (Show term, Show location, Show1 (Cell location)) => Show1 (Cache term location) where
  liftShowsPrec spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
      where spKey = liftShowsPrec spV slV
            slKey = liftShowList spV slV
            spPair = liftShowsPrec2 spV slV spHeap slHeap
            slPair = liftShowList2 spV slV spHeap slHeap
            spHeap = liftShowsPrec spV slV
            slHeap = liftShowList  spV slV
