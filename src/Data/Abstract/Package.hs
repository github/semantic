module Data.Abstract.Package where

import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable

type PackageName = Name

data Package term = Package
  { packageName        :: Maybe PackageName
  , packageVersion     :: Maybe Version
  , packageModules     :: ModuleTable [Module term]
  , packageEntryPoints :: ModuleTable (Maybe Name)
  }
  deriving (Eq, Functor, Ord, Show)

newtype Version = Version { versionString :: String }
  deriving (Eq, Ord, Show)


fromModules :: [Module term] -> Package term
fromModules []     = Package Nothing Nothing mempty mempty
fromModules (m:ms) = Package Nothing Nothing (ModuleTable.fromModules (m:ms)) entryPoints
  where entryPoints = ModuleTable.singleton (moduleName (moduleInfo m)) Nothing
