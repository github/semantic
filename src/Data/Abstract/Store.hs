{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Store where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Store l a = Store { unStore :: Monoidal.Map l (Cell l a) }
  deriving (Generic1)

deriving instance (Eq l, Eq (Cell l a)) => Eq (Store l a)
deriving instance (Ord l, Ord (Cell l a)) => Ord (Store l a)
deriving instance (Show l, Show (Cell l a)) => Show (Store l a)
instance (Eq l, Eq1 (Cell l)) => Eq1 (Store l) where liftEq = genericLiftEq
instance (Ord l, Ord1 (Cell l)) => Ord1 (Store l) where liftCompare = genericLiftCompare
instance (Show l, Show1 (Cell l)) => Show1 (Store l) where liftShowsPrec = genericLiftShowsPrec
deriving instance Foldable (Cell l) => Foldable (Store l)
deriving instance Functor (Cell l) => Functor (Store l)
deriving instance Traversable (Cell l) => Traversable (Store l)
deriving instance (Ord l, Semigroup (Cell l a)) => Semigroup (Store l a)
deriving instance (Ord l, Semigroup (Cell l a)) => Monoid (Store l a)
deriving instance (Ord l, Reducer a (Cell l a)) => Reducer (l, a) (Store l a)

-- | Look up the cell of values for an 'Address' in a 'Store', if any.
storeLookup :: Ord l => Address l a -> Store l a -> Maybe (Cell l a)
storeLookup (Address address) = Monoidal.lookup address . unStore

-- | Look up the list of values stored for a given address, if any.
storeLookupAll :: (Ord l, Foldable (Cell l)) => Address l a -> Store l a -> Maybe [a]
storeLookupAll address = fmap toList . storeLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
storeInsert :: (Ord l, Reducer a (Cell l a)) => Address l a -> a -> Store l a -> Store l a
storeInsert (Address address) value = flip snoc (address, value)

-- | The number of addresses extant in a 'Store'.
storeSize :: Store l a -> Int
storeSize = Monoidal.size . unStore

-- | Restrict a 'Store' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
storeRestrict :: Ord l => Store l a -> Live l a -> Store l a
storeRestrict (Store m) roots = Store (Monoidal.filterWithKey (\ address _ -> Address address `liveMember` roots) m)
