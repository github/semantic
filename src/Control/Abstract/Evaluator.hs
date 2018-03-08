{-# LANGUAGE ConstrainedClassMethods, TypeFamilies #-}
module Control.Abstract.Evaluator where

import Data.Abstract.Configuration
import Data.Abstract.ModuleTable
import Data.Abstract.Live
import Data.Abstract.Value
import Prelude hiding (fail)
import Prologue

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class MonadFail m => MonadEvaluator m where
  type TermFor m
  type ValueFor m

  -- | Retrieve the global environment.
  getGlobalEnv :: m (EnvironmentFor (ValueFor m))
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor (ValueFor m) -> EnvironmentFor (ValueFor m)) -> m ()

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor (ValueFor m))
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor (ValueFor m) -> EnvironmentFor (ValueFor m)) -> m a -> m a

  -- | Retrieve the heap.
  getStore :: m (StoreFor (ValueFor m))
  -- | Update the heap.
  modifyStore :: (StoreFor (ValueFor m) -> StoreFor (ValueFor m)) -> m ()
  putStore :: StoreFor (ValueFor m) -> m ()
  putStore = modifyStore . const

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (ModuleTable (ValueFor m))
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (ModuleTable (ValueFor m) -> ModuleTable (ValueFor m)) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (ModuleTable (TermFor m))
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable (TermFor m) -> ModuleTable (TermFor m)) -> m a -> m a

  -- | Retrieve the current root set.
  askRoots :: Ord (LocationFor (ValueFor m)) => m (Live (LocationFor (ValueFor m)) (ValueFor m))
  askRoots = pure mempty

  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor (ValueFor m)) => term -> m (Configuration (LocationFor (ValueFor m)) term (ValueFor m))
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getStore
