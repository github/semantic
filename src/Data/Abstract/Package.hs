{-# LANGUAGE TupleSections #-}
module Data.Abstract.Package where

import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Map as Map

type PackageName = Name

-- | Metadata for a package (name and version).
data PackageInfo = PackageInfo
  { packageName    :: PackageName
  , packageVersion :: Maybe Version
  }
  deriving (Eq, Ord, Show)

newtype Version = Version { versionString :: String }
  deriving (Eq, Ord, Show)

data PackageBody term = PackageBody
  { packageModules     :: ModuleTable [Module term]
  , packageEntryPoints :: ModuleTable (Maybe Name)
  }
  deriving (Eq, Functor, Ord, Show)


-- | A package represents the unit of dependency, i.e. something which can depend upon, or be depended upon by, other packages. Packages have modules and may have entry points from which evaluation can proceed.
data Package term = Package
  { packageInfo :: PackageInfo
  , packageBody :: PackageBody term
  }
  deriving (Eq, Functor, Ord, Show)

fromModules :: [Module term] -> PackageBody term
fromModules []     = PackageBody mempty mempty
fromModules (m:ms) = fromModulesWithEntryPoint (m : ms) (modulePath . moduleInfo <$> (m : ms))

fromModulesWithEntryPoint :: [Module term] -> [FilePath] -> PackageBody term
fromModulesWithEntryPoint ms paths = PackageBody (ModuleTable.fromModules ms) entryPoints
  where entryPoints = ModuleTable . Map.fromList $ (,Nothing) <$> paths

