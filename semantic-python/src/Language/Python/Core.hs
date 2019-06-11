module Language.Python.Core
( compile
) where

import Control.Monad.Fail
import Data.Core as Core
import Prelude hiding (fail)
import TreeSitter.Python.AST as Py

-- FIXME: we should really try not to fail
compile :: MonadFail m => Py.Module -> m Core
compile (Module Nothing) = pure Unit
compile (Module (Just _)) = pure Unit
