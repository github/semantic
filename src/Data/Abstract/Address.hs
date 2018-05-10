{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Address where

import Data.Abstract.FreeVariables
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | An abstract address with a location of @l@ pointing to a variable of type @a@.
newtype Address l a = Address { unAddress :: l }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq l => Eq1 (Address l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Address l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Address l) where liftShowsPrec = genericLiftShowsPrec


class Ord loc => Location loc where
  -- | The type into which stored values will be written for a given location type.
  type family Cell loc :: * -> *


-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Location Precise where
  type Cell Precise = Latest


-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Location Monovariant where
  type Cell Monovariant = Set


-- | A cell holding a single value. Writes will replace any prior value.
--   This is isomorphic to 'Last' from Data.Monoid, but is more convenient
--   because it has a 'Reducer' instance.
newtype Latest a = Latest { unLatest :: Maybe a }
  deriving (Eq, Foldable, Functor, Generic1, Lower, Ord, Show, Traversable)

instance Semigroup (Latest a) where
  a <> Latest Nothing = a
  _ <> b              = b

-- | 'Option' semantics rather than that of 'Maybe', which is broken.
instance Monoid (Latest a) where
  mappend = (<>)
  mempty  = Latest Nothing

instance Reducer a (Latest a) where
  unit = Latest . Just

instance Eq1 Latest where liftEq = genericLiftEq
instance Ord1 Latest where liftCompare = genericLiftCompare
instance Show1 Latest where liftShowsPrec = genericLiftShowsPrec
