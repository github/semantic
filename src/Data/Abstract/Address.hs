{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Data.Abstract.Address where

import Data.Abstract.FreeVariables
import Data.Semigroup.Reducer
import Prologue

-- | An abstract address with a location of @l@ pointing to a variable of type @a@.
newtype Address location value = Address { unAddress :: location }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq location => Eq1 (Address location) where liftEq = genericLiftEq
instance Ord location => Ord1 (Address location) where liftCompare = genericLiftCompare
instance Show location => Show1 (Address location) where liftShowsPrec = genericLiftShowsPrec


-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)


class (Foldable (Cell location), Ord location) => AbstractLocation location where
  -- | The type into which stored values will be written for a given location type.
  type family Cell location :: * -> *

instance AbstractLocation Precise where
  type Cell Precise = Latest

instance AbstractLocation Monovariant where
  type Cell Monovariant = Set


-- | A cell holding a single value. Writes will replace any prior value.
newtype Latest value = Latest { unLatest :: value }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Semigroup (Latest value) where
  _ <> a = a

instance Reducer value (Latest value) where
  unit = Latest
  cons _ = id
  snoc _ = unit

instance Eq1 Latest where liftEq = genericLiftEq
instance Ord1 Latest where liftCompare = genericLiftCompare
instance Show1 Latest where liftShowsPrec = genericLiftShowsPrec
