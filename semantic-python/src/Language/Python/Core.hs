module Language.Python.Core
( compileModule
) where

import Control.Applicative
import Data.Core as Core
import TreeSitter.Python.AST as Py

compileModule :: Alternative m => Py.Module -> m Core
compileModule (Module Nothing) = pure Unit
compileModule (Module (Just _)) = pure Unit
