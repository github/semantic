module Data.Abstract.Package where

import Data.Abstract.FreeVariables
import Data.Abstract.Module
import qualified Data.Map as Map

type PackageName = Name

data Package term = Package
  { packageName        :: Maybe PackageName
  , packageVersion     :: Maybe Version
  , packageModules     :: Map.Map ModuleName [Module term]
  , packageEntryPoints :: [EntryPoint]
  }
  deriving (Eq, Functor, Ord, Show)

data EntryPoint = EntryPoint
  { entryPointModuleName :: ModuleName
  , entryPointSymbol     :: Maybe Name
  }
  deriving (Eq, Ord, Show)

newtype Version = Version { versionString :: String }
  deriving (Eq, Ord, Show)
