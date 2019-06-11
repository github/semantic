{-# LANGUAGE DefaultSignatures #-}
module Language.Python.Core
( compile
) where

import Control.Monad.Fail
import Data.Core as Core
import Prelude hiding (fail)
import TreeSitter.Python.AST as Py

class Compile t where
  -- FIXME: we should really try not to fail
  compile :: MonadFail m => t -> m Core
  default compile :: (MonadFail m, Show t) => t -> m Core
  compile t = fail $ "compilation unimplemented for " <> show t

instance (Compile l, Compile r) => Compile (Either l r) where
  compile = either compile compile

instance Compile Py.Module where
  compile (Module Nothing) = pure Unit
  compile (Module (Just _)) = pure Unit
