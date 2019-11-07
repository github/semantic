{-# LANGUAGE DeriveFunctor #-}
module Data.Abstract.Package
  ( Package (..)
  , PackageInfo (..)
  , PackageName
  , Data.Abstract.Package.fromModules
  ) where

import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Map as Map
import Data.Abstract.Name

type PackageName = Name

-- | Metadata for a package (name and version).
data PackageInfo = PackageInfo
  { packageName        :: PackageName
  , packageResolutions :: Map.Map FilePath FilePath
  }
  deriving (Eq, Ord, Show)

-- | A package represents the unit of dependency, i.e. something which can depend upon, or be depended upon by, other packages. Packages have modules and may have entry points from which evaluation can proceed.
data Package term = Package
  { packageInfo :: PackageInfo
  , packageModules :: ModuleTable (Module term)
  }
  deriving (Eq, Functor, Ord, Show)

fromModules :: PackageName -> [Module term] -> Map.Map FilePath FilePath -> Package term
fromModules name modules resolutions = Package (PackageInfo name resolutions) (ModuleTable.fromModules modules)
