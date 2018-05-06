module Data.Abstract.Origin where

import Data.Abstract.Module
import Data.Abstract.Package

data Origin termInfo = Origin { originPackage :: PackageInfo, originModule :: ModuleInfo, originTermInfo :: Maybe termInfo }
  deriving (Eq, Ord, Show)

setTermInfo :: termInfo -> Origin termInfo -> Origin termInfo
setTermInfo termInfo o = o { originTermInfo = Just termInfo }
