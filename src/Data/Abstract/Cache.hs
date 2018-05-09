{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Cache where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Map.Monoidal as Monoidal
import Data.Semilattice.Lower
import Prologue

-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache location term value = Cache { unCache :: Monoidal.Map (Configuration location term value) (Set (value, Heap location value)) }
  deriving (Lower)

type Cacheable location term value = (Ord (Cell location value), Ord location, Ord term, Ord value)

deriving instance (Eq location, Eq term, Eq value, Eq (Cell location value)) => Eq (Cache location term value)
deriving instance Cacheable location term value => Ord (Cache location term value)
deriving instance (Show location, Show term, Show value, Show (Cell location value)) => Show (Cache location term value)
deriving instance Cacheable location term value => Semigroup (Cache location term value)
deriving instance Cacheable location term value => Monoid (Cache location term value)
deriving instance Cacheable location term value => Reducer (Configuration location term value, (value, Heap location value)) (Cache location term value)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: Cacheable location term value => Configuration location term value -> Cache location term value -> Maybe (Set (value, Heap location value))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: Cacheable location term value => Configuration location term value -> Set (value, Heap location value) -> Cache location term value -> Cache location term value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: Cacheable location term value => Configuration location term value -> (value, Heap location value) -> Cache location term value -> Cache location term value
cacheInsert = curry cons


instance (Eq location, Eq term, Eq1 (Cell location)) => Eq1 (Cache location term) where
  liftEq eqV (Cache c1) (Cache c2) = liftEq2 (liftEq eqV) (liftEq (liftEq2 eqV (liftEq eqV))) c1 c2

instance (Ord location, Ord term, Ord1 (Cell location)) => Ord1 (Cache location term) where
  liftCompare compareV (Cache c1) (Cache c2) = liftCompare2 (liftCompare compareV) (liftCompare (liftCompare2 compareV (liftCompare compareV))) c1 c2

instance (Show location, Show term, Show1 (Cell location)) => Show1 (Cache location term) where
  liftShowsPrec spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
      where spKey = liftShowsPrec spV slV
            slKey = liftShowList spV slV
            spPair = liftShowsPrec2 spV slV spHeap slHeap
            slPair = liftShowList2 spV slV spHeap slHeap
            spHeap = liftShowsPrec spV slV
            slHeap = liftShowList  spV slV
