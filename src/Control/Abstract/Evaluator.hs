{-# LANGUAGE ConstrainedClassMethods, DataKinds, KindSignatures, MultiParamTypeClasses, TypeFamilies #-}
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
class MonadFail (m effects) => MonadEvaluator (effects :: [* -> *]) m where
  type TermFor  m
  type ValueFor m

  -- | Retrieve the global environment.
  getGlobalEnv :: m effects (EnvironmentFor (ValueFor m))
  -- | Update the global environment.
  modifyGlobalEnv :: (EnvironmentFor (ValueFor m) -> EnvironmentFor (ValueFor m)) -> m effects ()

  -- | Retrieve the local environment.
  askLocalEnv :: m effects (EnvironmentFor (ValueFor m))
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor (ValueFor m) -> EnvironmentFor (ValueFor m)) -> m effects a -> m effects a

  -- | Retrieve the heap.
  getStore :: m effects (StoreFor (ValueFor m))
  -- | Update the heap.
  modifyStore :: (StoreFor (ValueFor m) -> StoreFor (ValueFor m)) -> m effects ()
  putStore :: StoreFor (ValueFor m) -> m effects ()
  putStore = modifyStore . const

  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m effects (ModuleTable (ValueFor m))
  -- | Update the table of evaluated modules.
  modifyModuleTable :: (ModuleTable (ValueFor m) -> ModuleTable (ValueFor m)) -> m effects ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m effects (ModuleTable (TermFor m))
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable (TermFor m) -> ModuleTable (TermFor m)) -> m effects a -> m effects a

  -- | Retrieve the current root set.
  askRoots :: Ord (LocationFor (ValueFor m)) => m effects (Live (LocationFor (ValueFor m)) (ValueFor m))
  askRoots = pure mempty

  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor (ValueFor m)) => term -> m effects (Configuration (LocationFor (ValueFor m)) term (ValueFor m))
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getStore
