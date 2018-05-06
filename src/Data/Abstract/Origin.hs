module Data.Abstract.Origin where

import Data.Abstract.Module
import Data.Abstract.Package

data Origin term = Origin
  { originPackage :: {-# UNPACK #-} PackageInfo
  , originModule  :: {-# UNPACK #-} ModuleInfo
  , originTerm    :: term
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
