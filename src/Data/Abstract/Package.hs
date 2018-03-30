module Data.Abstract.Package where

import Data.Abstract.Module
import qualified Data.Map as Map

data Package term = Package
  { packageName :: String
  , packageModules :: Map.Map ModuleName [Module term]
  }
