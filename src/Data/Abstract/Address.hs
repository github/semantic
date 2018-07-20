{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Address
  ( Precise (..)
  , Located (..)
  , Latest (..)
  , All (..)
  , Monovariant (..)
  ) where

import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Abstract.Package (PackageInfo)
import Data.Monoid (Last(..))
import Data.Semigroup.Reducer
import Data.Span
import Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unMonovariant


data Located address = Located
  { address        :: address
  , addressPackage :: {-# UNPACK #-} !PackageInfo
  , addressModule  :: !ModuleInfo
  , addressName    :: Name
  , addressSpan    :: Span
  }
  deriving (Eq, Ord, Show)


-- | A cell holding a single value. Writes will replace any prior value.
--
--   This is equivalent to 'Data.Monoid.Last', but with a 'Show' instance designed to minimize the amount of text we have to scroll past in ghci.
newtype Latest value = Latest { unLatest :: Last value }
  deriving (Eq, Foldable, Functor, Lower, Monoid, Semigroup, Ord, Traversable)

instance Reducer value (Latest value) where
  unit = Latest . unit . Just

instance Show value => Show (Latest value) where
  showsPrec d = showsPrec d . getLast . unLatest


-- | A cell holding all values written to its address.
--
--   This is equivalent to 'Set', but with a 'Show' instance designed to minimize the amount of text we have to scroll past in ghci.
newtype All value = All { unAll :: Set value }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Reducer value, Semigroup)

instance Show value => Show (All value) where
  showsPrec d = showsPrec d . Set.toList . unAll
