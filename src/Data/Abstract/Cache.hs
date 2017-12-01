{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Cache where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Store
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup
import Data.Set
import Data.Map as Map

newtype Cache l t v = Cache { unCache :: Map.Map (Configuration l t v) (Set (v, Store l v)) }

deriving instance (Eq l, Eq t, Eq v, Eq (Cell l v)) => Eq (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Ord (Cache l t v)
deriving instance (Show l, Show t, Show v, Show (Cell l v)) => Show (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Semigroup (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Monoid (Cache l t v)

cacheLookup :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> Cache l t v -> Maybe (Set (v, Store l v))
cacheLookup key = Map.lookup key . unCache

cacheSet :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> Set (v, Store l v) -> Cache l t v -> Cache l t v
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> (v, Store l v) -> Cache l t v -> Cache l t v
cacheInsert = (((Cache .) . (. unCache)) .) . (. point) . Map.insertWith (<>)


instance (Eq l, Eq1 (Cell l)) => Eq2 (Cache l) where
  liftEq2 eqT eqV (Cache a) (Cache b) = liftEq2 (liftEq2 eqT eqV) (liftEq (liftEq2 eqV (liftEq eqV))) a b

instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Cache l t) where
  liftEq = liftEq2 (==)


instance (Ord l, Ord1 (Cell l)) => Ord2 (Cache l) where
  liftCompare2 compareT compareV (Cache a) (Cache b) = liftCompare2 (liftCompare2 compareT compareV) (liftCompare (liftCompare2 compareV (liftCompare compareV))) a b

instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Cache l t) where
  liftCompare = liftCompare2 compare


instance (Show l, Show1 (Cell l)) => Show2 (Cache l) where
  liftShowsPrec2 spT slT spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec2 spT slT spV slV
          slKey = liftShowList2 spT slT spV slV
          spPair = liftShowsPrec2 spV slV spStore slStore
          slPair = liftShowList2 spV slV spStore slStore
          spStore = liftShowsPrec spV slV
          slStore = liftShowList  spV slV

instance (Show l, Show t, Show1 (Cell l)) => Show1 (Cache l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
