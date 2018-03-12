{-# LANGUAGE ConstrainedClassMethods, FunctionalDependencies #-}
module Control.Abstract.Evaluator where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.ModuleTable
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Semigroup.Reducer
import Prelude hiding (fail)
import Prologue

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class ( MonadEnvironment value m
      , MonadFail m
      , MonadModuleTable term value m
      , MonadStore value m
      )
      => MonadEvaluator term value m | m -> term, m -> value where
  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord (LocationFor value) => term -> m (ConfigurationFor term value)

class Monad m => MonadEnvironment value m | m -> value where
  -- | Retrieve the global environment.
  getGlobalEnv :: m (EnvironmentFor value)
  -- | Set the global environment
  putGlobalEnv :: EnvironmentFor value -> m ()

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

-- | Update the global environment.
modifyGlobalEnv :: MonadEvaluator term value m => (EnvironmentFor value -> EnvironmentFor value) -> m  ()
modifyGlobalEnv f = do
  env <- getGlobalEnv
  putGlobalEnv $! f env


class Monad m => MonadStore value m | m -> value where
  -- | Retrieve the heap.
  getStore :: m (StoreFor value)
  -- | Set the heap.
  putStore :: StoreFor value -> m ()

-- | Update the heap.
modifyStore :: MonadStore value m => (StoreFor value -> StoreFor value) -> m ()
modifyStore f = do
  s <- getStore
  putStore $! f s

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord (LocationFor value)
          , MonadStore value m
          , Reducer value (CellFor value)
          )
          => Address (LocationFor value) value
          -> value
          -> m ()
assign address = modifyStore . storeInsert address


class Monad m => MonadModuleTable term value m | m -> term, m -> value where
  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (ModuleTable (EnvironmentFor value))
  -- | Set the table of evaluated modules.
  putModuleTable :: ModuleTable (EnvironmentFor value) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (ModuleTable term)
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable term -> ModuleTable term) -> m a -> m a

modifyModuleTable :: MonadModuleTable term value m => (ModuleTable (EnvironmentFor value) -> ModuleTable (EnvironmentFor value)) -> m ()
modifyModuleTable f = do
  table <- getModuleTable
  putModuleTable $! f table
