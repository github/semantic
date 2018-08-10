module Data.Abstract.Address
  ( Precise (..)
  , Located (..)
  , Monovariant (..)
  ) where

import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Abstract.Package (PackageInfo)
import Data.Span
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
