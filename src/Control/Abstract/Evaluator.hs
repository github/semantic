{-# LANGUAGE ConstrainedClassMethods, FunctionalDependencies #-}
module Control.Abstract.Evaluator
( MonadEvaluator(..)
, MonadEnvironment(..)
, modifyGlobalEnv
, MonadHeap(..)
, modifyHeap
, assign
, MonadModuleTable(..)
, modifyModuleTable
, MonadControl(..)
) where

import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.ModuleTable
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

-- | Update the global environment.
modifyGlobalEnv :: MonadEnvironment value m => (EnvironmentFor value -> EnvironmentFor value) -> m  ()
modifyGlobalEnv f = do
  env <- getGlobalEnv
  putGlobalEnv $! f env


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


-- | The type of labels.
--   TODO: This should be rolled into 'Name' and tracked in the environment, both so that we can abstract over labels like any other location, and so that we can garbage collect unreachable labels.
type Label = Int

-- | A 'Monad' abstracting jumps in imperative control.
class Monad m => MonadControl term m where
  -- | Allocate a 'Label' for the given @term@.
  --
  --   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
  label :: term -> m Label
  -- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
  goto :: Label -> m term
