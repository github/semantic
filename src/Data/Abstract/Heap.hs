{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Heap where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Heap l a = Heap { unStore :: Monoidal.Map l (Cell l a) }
  deriving (Generic1)

deriving instance (Eq l, Eq (Cell l a)) => Eq (Heap l a)
deriving instance (Ord l, Ord (Cell l a)) => Ord (Heap l a)
deriving instance (Show l, Show (Cell l a)) => Show (Heap l a)
instance (Eq l, Eq1 (Cell l)) => Eq1 (Heap l) where liftEq = genericLiftEq
instance (Ord l, Ord1 (Cell l)) => Ord1 (Heap l) where liftCompare = genericLiftCompare
instance (Show l, Show1 (Cell l)) => Show1 (Heap l) where liftShowsPrec = genericLiftShowsPrec
deriving instance Foldable (Cell l) => Foldable (Heap l)
deriving instance Functor (Cell l) => Functor (Heap l)
deriving instance Traversable (Cell l) => Traversable (Heap l)
deriving instance (Ord l, Semigroup (Cell l a)) => Semigroup (Heap l a)
deriving instance (Ord l, Semigroup (Cell l a)) => Monoid (Heap l a)
deriving instance (Ord l, Reducer a (Cell l a)) => Reducer (l, a) (Heap l a)

-- | Look up the cell of values for an 'Address' in a 'Heap', if any.
heapLookup :: Ord l => Address l a -> Heap l a -> Maybe (Cell l a)
heapLookup (Address address) = Monoidal.lookup address . unStore

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: (Ord l, Foldable (Cell l)) => Address l a -> Heap l a -> Maybe [a]
heapLookupAll address = fmap toList . heapLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
heapInsert :: (Ord l, Reducer a (Cell l a)) => Address l a -> a -> Heap l a -> Heap l a
heapInsert (Address address) value = flip snoc (address, value)

-- | The number of addresses extant in a 'Heap'.
heapSize :: Heap l a -> Int
heapSize = Monoidal.size . unStore

-- | Restrict a 'Heap' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Ord l => Heap l a -> Live l a -> Heap l a
heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> Address address `liveMember` roots) m)
