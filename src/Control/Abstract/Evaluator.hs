{-# LANGUAGE ConstrainedClassMethods, FunctionalDependencies #-}
module Control.Abstract.Evaluator
( MonadEvaluator(..)
, MonadEnvironment(..)
, modifyGlobalEnv
, MonadStore(..)
, modifyStore
, assign
, MonadModuleTable(..)
, modifyModuleTable
) where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
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

-- | A 'Monad' abstracting local and global environments.
class Monad m => MonadEnvironment value m | m -> value where
  -- | Retrieve the global environment.
  getGlobalEnv :: m (EnvironmentFor value)
  -- | Set the global environment
  putGlobalEnv :: EnvironmentFor value -> m ()
  withGlobalEnv :: EnvironmentFor value -> m a -> m a

  -- | Add an export to the global export state.
  addExport :: Name -> (Name, Maybe (Address (LocationFor value) value)) -> m ()
  -- | Get the global export state.
  getExports :: m (Map Name (Name, Maybe (Address (LocationFor value) value)))
  -- | Sets the exports state to the given map for the lifetime of the given action.
  withExports :: Map Name (Name, Maybe (Address (LocationFor value) value)) -> m a -> m a

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

  -- | Look a 'Name' up in the local environment.
  lookupLocalEnv :: Name -> m (Maybe (Address (LocationFor value) value))
  lookupLocalEnv name = envLookup name <$> askLocalEnv

  lookupWith :: (Address (LocationFor value) value -> m value) -> Name -> m (Maybe value)
  lookupWith with name = do
    addr <- lookupLocalEnv name
    maybe (pure Nothing) (fmap Just . with) addr

-- | Update the global environment.
modifyGlobalEnv :: MonadEnvironment value m => (EnvironmentFor value -> EnvironmentFor value) -> m  ()
modifyGlobalEnv f = do
  env <- getGlobalEnv
  putGlobalEnv $! f env


-- | A 'Monad' abstracting a heap of values.
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


-- | A 'Monad' abstracting tables of modules available for import.
class Monad m => MonadModuleTable term value m | m -> term, m -> value where
  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (ModuleTable (EnvironmentFor value))
  -- | Set the table of evaluated modules.
  putModuleTable :: ModuleTable (EnvironmentFor value) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (ModuleTable [term])
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable [term] -> ModuleTable [term]) -> m a -> m a

-- | Update the evaluated module table.
modifyModuleTable :: MonadModuleTable term value m => (ModuleTable (EnvironmentFor value) -> ModuleTable (EnvironmentFor value)) -> m ()
modifyModuleTable f = do
  table <- getModuleTable
  putModuleTable $! f table
