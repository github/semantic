{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Address where

import Data.Abstract.FreeVariables
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Package (PackageInfo)
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Data.Set as Set
import Prologue

-- | An abstract address with a @location@ pointing to a variable of type @value@.
newtype Address location value = Address { unAddress :: location }
  deriving (Eq, Ord)

instance Eq   location => Eq1   (Address location) where liftEq          _ a b = unAddress a    ==     unAddress b
instance Ord  location => Ord1  (Address location) where liftCompare     _ a b = unAddress a `compare` unAddress b
instance Show location => Show1 (Address location) where liftShowsPrec _ _     = showsPrec

instance Show location => Show (Address location value) where
  showsPrec d = showsPrec d . unAddress


-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unName . unMonovariant


data Located location = Located
  { location        :: location
  , locationPackage :: {-# UNPACK #-} !PackageInfo
  , locationModule  :: !ModuleInfo
  }
  deriving (Eq, Ord, Show)


-- | A cell holding all values written to its address.
--
--   This is equivalent to 'Set', but with a 'Show' instance designed to minimize the amount of text we have to scroll past in ghci.
newtype All value = All { unAll :: Set value }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Reducer value, Semigroup)

instance Show value => Show (All value) where
  showsPrec d = showsPrec d . Set.toList . unAll
