module Data.Abstract.Package where

import Data.Abstract.FreeVariables
import Data.Abstract.Module
import qualified Data.Map as Map

type PackageName = Name

data Package term = Package
  { packageName        :: PackageName
  , packageVersion     :: Maybe Version
  , packageModules     :: Map.Map ModuleName [Module term]
  , packageEntryPoints :: [EntryPoint]
  }

data EntryPoint = EntryPoint
  { entryPointModuleName :: ModuleName
  , entryPointSymbol     :: Maybe Name
  }

newtype Version = Version { versionString :: String }
