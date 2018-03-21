module Data.Abstract.Module
( Module(..)
, ModuleName
) where

import Data.Abstract.FreeVariables

type ModuleName = Name

data Module term = Module { moduleName :: ModuleName, modulePath :: FilePath, moduleTerm :: term }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
