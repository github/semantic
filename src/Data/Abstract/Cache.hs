{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Cache where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Map.Monoidal as Monoidal
import Prologue

-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache l t v = Cache { unCache :: Monoidal.Map (Configuration l t v) (Set (v, Heap l v)) }

deriving instance (Eq l, Eq t, Eq v, Eq (Cell l v)) => Eq (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Ord (Cache l t v)
deriving instance (Show l, Show t, Show v, Show (Cell l v)) => Show (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Semigroup (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Monoid (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Reducer (Configuration l t v, (v, Heap l v)) (Cache l t v)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> Cache l t v -> Maybe (Set (v, Heap l v))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> Set (v, Heap l v) -> Cache l t v -> Cache l t v
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> (v, Heap l v) -> Cache l t v -> Cache l t v
cacheInsert = curry cons


instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Cache l t) where
  liftEq eqV (Cache c1) (Cache c2) = liftEq2 (liftEq eqV) (liftEq (liftEq2 eqV (liftEq eqV))) c1 c2

instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Cache l t) where
  liftCompare compareV (Cache c1) (Cache c2) = liftCompare2 (liftCompare compareV) (liftCompare (liftCompare2 compareV (liftCompare compareV))) c1 c2

instance (Show l, Show t, Show1 (Cell l)) => Show1 (Cache l t) where
  liftShowsPrec spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
      where spKey = liftShowsPrec spV slV
            slKey = liftShowList spV slV
            spPair = liftShowsPrec2 spV slV spHeap slHeap
            slPair = liftShowList2 spV slV spHeap slHeap
            spHeap = liftShowsPrec spV slV
            slHeap = liftShowList  spV slV
