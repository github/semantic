{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Abstract.Address where

import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Package (PackageInfo)
import Prologue

-- | An abstract address with a @location@ pointing to a variable of type @value@.
newtype Address location value = Address { unAddress :: location }
  deriving (Eq, Ord)

instance Eq   location => Eq1   (Address location) where liftEq          _ a b = unAddress a    ==     unAddress b
instance Ord  location => Ord1  (Address location) where liftCompare     _ a b = unAddress a `compare` unAddress b
instance Show location => Show1 (Address location) where liftShowsPrec _ _     = showsPrec

instance Show location => Show (Address location value) where
  showsPrec d = showsPrec d . unAddress


data Located location = Located
  { location        :: location
  , locationPackage :: {-# UNPACK #-} !PackageInfo
  , locationModule  :: !ModuleInfo
  }
  deriving (Eq, Ord, Show)
