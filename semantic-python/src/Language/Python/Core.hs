module Language.Python.Core
( compileModule
) where

import Control.Monad.Fail
import Data.Core as Core
import Prelude hiding (fail)
import TreeSitter.Python.AST as Py

compileModule :: MonadFail m => Py.Module -> m Core
compileModule (Module Nothing) = pure Unit
compileModule (Module (Just _)) = pure Unit
