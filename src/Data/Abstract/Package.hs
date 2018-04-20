module Data.Abstract.Package where

import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable

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
  , packagePrelude     :: Maybe (Module term)
  , packageEntryPoints :: ModuleTable (Maybe Name)
  }
  deriving (Eq, Functor, Ord, Show)


-- | A package represents the unit of dependency, i.e. something which can depend upon, or be depended upon by, other packages. Packages have modules and may have entry points from which evaluation can proceed.
data Package term = Package
  { packageInfo :: PackageInfo
  , packageBody :: PackageBody term
  }
  deriving (Eq, Functor, Ord, Show)

fromModules :: PackageName -> Maybe Version -> Maybe (Module term) -> [Module term] -> Package term
fromModules name version prelude = Package (PackageInfo name version) . go prelude
  where
    go :: Maybe (Module term) -> [Module term] -> PackageBody term
    go p []     = PackageBody mempty p mempty
    go p (m:ms) = PackageBody (ModuleTable.fromModules (m : ms)) p entryPoints
      where
        entryPoints = ModuleTable.singleton path Nothing
        path = modulePath (moduleInfo m)
