{-# LANGUAGE ConstrainedClassMethods, DataKinds, KindSignatures, MultiParamTypeClasses #-}
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
class MonadFail (m term value effects) => MonadEvaluator term value (effects :: [* -> *]) m where
  -- | Retrieve the global environment.
  getGlobalEnv :: m term value effects (EnvironmentFor value)
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m term value effects  ()

  -- | Retrieve the local environment.
  askLocalEnv :: m term value effects (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m term value effects a -> m term value effects a

  -- | Retrieve the heap.
  getStore :: m term value effects (StoreFor value)
  -- | Update the heap.
  modifyStore :: (StoreFor value -> StoreFor value) -> m term value effects ()
  putStore :: StoreFor value -> m term value effects ()
  putStore = modifyStore . const

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m term value effects (ModuleTable value)
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (ModuleTable value -> ModuleTable value) -> m term value effects ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m term value effects (ModuleTable term)
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable term -> ModuleTable term) -> m term value effects a -> m term value effects a

  -- | Retrieve the current root set.
  askRoots :: Ord (LocationFor value) => m term value effects (Live (LocationFor value) value)
  askRoots = pure mempty

  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor value) => term -> m term value effects (Configuration (LocationFor value) term value)
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getStore
