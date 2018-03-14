{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Data.Abstract.Store where

import Data.Abstract.Address
import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Store location value = Store { unStore :: Monoidal.Map location (Cell location value) }
  deriving (Generic1)

deriving instance (Eq location, Eq (Cell location value)) => Eq (Store location value)
deriving instance (Ord location, Ord (Cell location value)) => Ord (Store location value)
deriving instance (Show location, Show (Cell location value)) => Show (Store location value)
instance (Eq location, Eq1 (Cell location)) => Eq1 (Store location) where liftEq = genericLiftEq
instance (Ord location, Ord1 (Cell location)) => Ord1 (Store location) where liftCompare = genericLiftCompare
instance (Show location, Show1 (Cell location)) => Show1 (Store location) where liftShowsPrec = genericLiftShowsPrec
deriving instance Foldable (Cell location) => Foldable (Store location)
deriving instance Functor (Cell location) => Functor (Store location)
deriving instance Traversable (Cell location) => Traversable (Store location)
deriving instance (Ord location, Semigroup (Cell location value)) => Semigroup (Store location value)
deriving instance (Ord location, Semigroup (Cell location value)) => Monoid (Store location value)
deriving instance (Ord location, Reducer value (Cell location value)) => Reducer (location, value) (Store location value)

-- | Look up the cell of values for an 'Address' in a 'Store', if any.
storeLookup :: Ord location => Address location value -> Store location value -> Maybe (Cell location value)
storeLookup (Address address) = Monoidal.lookup address . unStore

-- | Look up the list of values stored for a given address, if any.
storeLookupAll :: (Ord location, Foldable (Cell location)) => Address location value -> Store location value -> Maybe [value]
storeLookupAll address = fmap toList . storeLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
storeInsert :: (Ord location, Reducer value (Cell location value)) => Address location value -> value -> Store location value -> Store location value
storeInsert (Address address) value = flip snoc (address, value)

-- | The number of addresses extant in a 'Store'.
storeSize :: Store location value -> Int
storeSize = Monoidal.size . unStore

-- | Restrict a 'Store' to only those 'Address'es in the given 'Live' set (in essence garbage collecting the rest).
storeRestrict :: Ord location => Store location value -> Live location value -> Store location value
storeRestrict (Store m) roots = Store (Monoidal.filterWithKey (\ address _ -> Address address `liveMember` roots) m)
