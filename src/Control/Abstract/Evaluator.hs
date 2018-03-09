{-# LANGUAGE ConstrainedClassMethods, DataKinds, FunctionalDependencies, KindSignatures #-}
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
class MonadFail m => MonadEvaluator term value m | m -> term, m -> value where
  -- | Retrieve the global environment.
  getGlobalEnv :: m (EnvironmentFor value)
  -- | Set the global environment
  putGlobalEnv :: EnvironmentFor value -> m ()
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m  ()

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

  -- | Retrieve the heap.
  getStore :: m (StoreFor value)
  -- | Update the heap.
  modifyStore :: (StoreFor value -> StoreFor value) -> m ()
  putStore :: StoreFor value -> m ()
  putStore = modifyStore . const

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (ModuleTable (EnvironmentFor value))
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (ModuleTable (EnvironmentFor value) -> ModuleTable (EnvironmentFor value)) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (ModuleTable term)
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable term -> ModuleTable term) -> m a -> m a

  -- | Retrieve the current root set.
  askRoots :: Ord (LocationFor value) => m (Live (LocationFor value) value)
  askRoots = pure mempty

  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor value) => term -> m (Configuration (LocationFor value) term value)
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getStore
