{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Store where

import Data.Abstract.Address
import Data.Abstract.Live
import Data.Foldable (toList)
import Data.Functor.Classes.Generic
import qualified Data.Map as Map
import Data.Pointed
import Data.Semigroup
import GHC.Generics

-- | A map of addresses onto cells holding their values.
newtype Store l a = Store { unStore :: Map.Map l (Cell l a) }
  deriving (Generic1, Monoid, Semigroup)

deriving instance (Eq l, Eq (Cell l a)) => Eq (Store l a)
deriving instance (Ord l, Ord (Cell l a)) => Ord (Store l a)
deriving instance (Show l, Show (Cell l a)) => Show (Store l a)
instance (Eq l, Eq1 (Cell l)) => Eq1 (Store l) where liftEq = genericLiftEq
instance (Ord l, Ord1 (Cell l)) => Ord1 (Store l) where liftCompare = genericLiftCompare
instance (Show l, Show1 (Cell l)) => Show1 (Store l) where liftShowsPrec = genericLiftShowsPrec
deriving instance Foldable (Cell l) => Foldable (Store l)
deriving instance Functor (Cell l) => Functor (Store l)
deriving instance Traversable (Cell l) => Traversable (Store l)

-- | Look up the cell of values for an 'Address' in a 'Store', if any.
storeLookup :: Ord l => Address l a -> Store l a -> Maybe (Cell l a)
storeLookup (Address address) = Map.lookup address . unStore

-- | Look up the list of values stored for a given address, if any.
storeLookupAll :: (Ord l, Foldable (Cell l)) => Address l a -> Store l a -> Maybe [a]
storeLookupAll address = fmap toList . storeLookup address

storeInsert :: (Ord l, Semigroup (Cell l a), Pointed (Cell l)) => Address l a -> a -> Store l a -> Store l a
storeInsert (Address address) value = Store . Map.insertWith (<>) address (point value) . unStore

storeSize :: Store l a -> Int
storeSize = Map.size . unStore

storeRestrict :: Ord l => Store l a -> Live l a -> Store l a
storeRestrict (Store m) roots = Store (Map.filterWithKey (\ address _ -> Address address `liveMember` roots) m)
