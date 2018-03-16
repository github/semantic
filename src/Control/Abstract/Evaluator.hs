{-# LANGUAGE ConstrainedClassMethods, FunctionalDependencies #-}
module Control.Abstract.Evaluator
( MonadEvaluator(..)
, MonadEnvironment(..)
, modifyGlobalEnv
, modifyExports
, addExport
, MonadHeap(..)
, modifyHeap
, lookupHeap
, assign
, MonadModuleTable(..)
, modifyModuleTable
, MonadControl(..)
) where

import Data.Abstract.Address
import Data.Abstract.Configuration
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Exports
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.ModuleTable
import Data.Abstract.Value
import Data.Semigroup.Reducer
import Prelude
import Prologue

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class ( MonadControl term m
      , MonadEnvironment value m
      , MonadFail m
      , MonadModuleTable term value m
      , MonadHeap value m
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
  -- | Sets the global environment for the lifetime of the given action.
  withGlobalEnv :: EnvironmentFor value -> m a -> m a

  -- | Get the global export state.
  getExports :: m (ExportsFor value)
  -- | Set the global export state.
  putExports :: ExportsFor value -> m ()
  -- | Sets the global export state for the lifetime of the given action.
  withExports :: ExportsFor value -> m a -> m a

  -- | Retrieve the local environment.
  askLocalEnv :: m (EnvironmentFor value)
  -- | Run an action with a locally-modified environment.
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

  -- | Look a 'Name' up in the local environment.
  lookupLocalEnv :: Name -> m (Maybe (Address (LocationFor value) value))
  lookupLocalEnv name = Env.lookup name <$> askLocalEnv

  -- | Look up a 'Name' in the local environment, running an action with the resolved address (if any).
  lookupWith :: (Address (LocationFor value) value -> m value) -> Name -> m (Maybe value)
  lookupWith with name = do
    addr <- lookupLocalEnv name
    maybe (pure Nothing) (fmap Just . with) addr

-- | Update the global environment.
-- TODO: RENAME ME BECAUSE MY NAME IS A LIE
modifyGlobalEnv :: MonadEnvironment value m => (EnvironmentFor value -> EnvironmentFor value) -> m ()
modifyGlobalEnv f = do
  env <- getGlobalEnv
  putGlobalEnv $! f env

-- | Update the global export state.
modifyExports :: MonadEnvironment value m => (ExportsFor value -> ExportsFor value) -> m ()
modifyExports f = do
  exports <- getExports
  putExports $! f exports

-- | Add an export to the global export state.
addExport :: MonadEnvironment value m => Name -> Name -> Maybe (Address (LocationFor value) value) -> m ()
addExport name alias = modifyExports . exportInsert name alias

-- | A 'Monad' abstracting a heap of values.
class Monad m => MonadHeap value m | m -> value where
  -- | Retrieve the heap.
  getHeap :: m (HeapFor value)
  -- | Set the heap.
  putHeap :: HeapFor value -> m ()

-- | Update the heap.
modifyHeap :: MonadHeap value m => (HeapFor value -> HeapFor value) -> m ()
modifyHeap f = do
  s <- getHeap
  putHeap $! f s

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (MonadHeap value m, Ord (LocationFor value)) => Address (LocationFor value) value -> m (Maybe (CellFor value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord (LocationFor value)
          , MonadHeap value m
          , Reducer value (CellFor value)
          )
       => Address (LocationFor value) value
       -> value
       -> m ()
assign address = modifyHeap . heapInsert address


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


-- | A 'Monad' abstracting jumps in imperative control.
class Monad m => MonadControl term m where
  -- | Allocate a 'Label' for the given @term@.
  --
  --   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
  label :: term -> m Label
  -- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
  goto :: Label -> m term
