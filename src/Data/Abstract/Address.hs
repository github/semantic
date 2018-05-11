{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Address where

import Data.Abstract.FreeVariables
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Package (PackageInfo)
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | An abstract address with a @location@ pointing to a variable of type @value@.
newtype Address location value = Address location
  deriving (Eq, Ord, Show)

unAddress :: Address location value -> location
unAddress (Address location) = location

instance Eq   location => Eq1   (Address location) where liftEq          _ a b = unAddress a    ==     unAddress b
instance Ord  location => Ord1  (Address location) where liftCompare     _ a b = unAddress a `compare` unAddress b
instance Show location => Show1 (Address location) where liftShowsPrec _ _     = showsPrec


class Location location where
  -- | The type into which stored values will be written for a given location type.
  type family Cell location :: * -> *


-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise Int
  deriving (Eq, Ord, Show)

instance Location Precise where
  type Cell Precise = Latest


-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant Name
  deriving (Eq, Ord, Show)

instance Location Monovariant where
  type Cell Monovariant = Set


data Located location = Located
  { location        :: location
  , locationPackage :: {-# UNPACK #-} !PackageInfo
  , locationModule  :: !ModuleInfo
  }
  deriving (Eq, Ord, Show)

instance Location (Located location) where
  type Cell (Located location) = Cell location


-- | A cell holding a single value. Writes will replace any prior value.
--   This is isomorphic to 'Last' from Data.Monoid, but is more convenient
--   because it has a 'Reducer' instance.
newtype Latest value = Latest (Maybe value)
  deriving (Eq, Foldable, Functor, Generic1, Lower, Ord, Show, Traversable)

unLatest :: Latest value -> Maybe value
unLatest (Latest value) = value

instance Semigroup (Latest value) where
  a <> Latest Nothing = a
  _ <> b              = b

-- | 'Option' semantics rather than that of 'Maybe', which is broken.
instance Monoid (Latest value) where
  mappend = (<>)
  mempty  = Latest Nothing

instance Reducer value (Latest value) where
  unit = Latest . Just

instance Eq1   Latest where liftEq        = genericLiftEq
instance Ord1  Latest where liftCompare   = genericLiftCompare
instance Show1 Latest where liftShowsPrec = genericLiftShowsPrec
