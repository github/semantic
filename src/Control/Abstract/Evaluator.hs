{-# LANGUAGE ConstrainedClassMethods, DataKinds, FunctionalDependencies, RankNTypes, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Evaluator
  ( MonadEvaluator(..)
  , MonadEnvironment(..)
  , modifyEnv
  , modifyExports
  , addExport
  , fullEnvironment
  , MonadHeap(..)
  , modifyHeap
  , localize
  , lookupHeap
  , assign
  , MonadModuleTable(..)
  , modifyModuleTable
  , modifyLoadStack
  , MonadControl(..)
  , MonadResume(..)
  , MonadExc(..)
  ) where

import Control.Effect
import Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Resumable as Resumable
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Environment as Env
import Data.Abstract.Exports as Export
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Data.Semigroup.Reducer
import Prologue hiding (throwError)

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class ( MonadControl term m
      , MonadEnvironment location value m
      , MonadFail m
      , MonadModuleTable location term value m
      , MonadHeap location value m
      )
      => MonadEvaluator location term value m | m -> location, m -> term, m -> value where
  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord location => term -> m (Configuration location term value)

-- | A 'Monad' abstracting local and global environments.
class Monad m => MonadEnvironment location value m | m -> value, m -> location where
  -- | Retrieve the environment.
  getEnv :: m (Environment location value)
  -- | Set the environment.
  putEnv :: Environment location value -> m ()
  -- | Sets the environment for the lifetime of the given action.
  withEnv :: Environment location value -> m a -> m a

  -- | Retrieve the default environment.
  defaultEnvironment :: m (Environment location value)

  -- | Set the default environment for the lifetime of an action.
  --   Usually only invoked in a top-level evaluation function.
  withDefaultEnvironment :: Environment location value -> m a -> m a

  -- | Get the global export state.
  getExports :: m (Exports location value)
  -- | Set the global export state.
  putExports :: Exports location value -> m ()
  -- | Sets the global export state for the lifetime of the given action.
  withExports :: Exports location value -> m a -> m a

  -- | Run an action with a locally-modified environment.
  localEnv :: (Environment location value -> Environment location value) -> m a -> m a

  -- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
  lookupEnv :: Name -> m (Maybe (Address location value))
  lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)

  -- | Look up a 'Name' in the environment, running an action with the resolved address (if any).
  lookupWith :: (Address location value -> m a) -> Name -> m (Maybe a)
  lookupWith with name = do
    addr <- lookupEnv name
    maybe (pure Nothing) (fmap Just . with) addr

-- | Run a computation in a new local environment.
localize :: MonadEnvironment location value m => m a -> m a
localize = localEnv id

-- | Update the global environment.
modifyEnv :: MonadEnvironment location value m => (Environment location value -> Environment location value) -> m ()
modifyEnv f = do
  env <- getEnv
  putEnv $! f env

-- | Update the global export state.
modifyExports :: MonadEnvironment location value m => (Exports location value -> Exports location value) -> m ()
modifyExports f = do
  exports <- getExports
  putExports $! f exports

-- | Add an export to the global export state.
addExport :: MonadEnvironment location value m => Name -> Name -> Maybe (Address location value) -> m ()
addExport name alias = modifyExports . Export.insert name alias

-- | Obtain an environment that is the composition of the current and default environments.
--   Useful for debugging.
fullEnvironment :: MonadEnvironment location value m => m (Environment location value)
fullEnvironment = mappend <$> getEnv <*> defaultEnvironment

-- | A 'Monad' abstracting a heap of values.
class Monad m => MonadHeap location value m | m -> value, m -> location where
  -- | Retrieve the heap.
  getHeap :: m (Heap location value)
  -- | Set the heap.
  putHeap :: Heap location value -> m ()

-- | Update the heap.
modifyHeap :: MonadHeap location value m => (Heap location value -> Heap location value) -> m ()
modifyHeap f = do
  s <- getHeap
  putHeap $! f s

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (MonadHeap location value m, Ord location) => Address location value -> m (Maybe (Cell location value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord location
          , MonadHeap location value m
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> m ()
assign address = modifyHeap . heapInsert address


-- | A 'Monad' abstracting tables of modules available for import.
class Monad m => MonadModuleTable location term value m | m -> location, m -> term, m -> value where
  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m (ModuleTable (Environment location value, value))
  -- | Set the table of evaluated modules.
  putModuleTable :: ModuleTable (Environment location value, value) -> m ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m (ModuleTable [Module term])
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable [Module term] -> ModuleTable [Module term]) -> m a -> m a

  -- | Retrieve the module load stack
  getLoadStack :: m LoadStack
  -- | Set the module load stack
  putLoadStack :: LoadStack -> m ()

  -- | Get the currently evaluating 'ModuleInfo'.
  currentModule :: m ModuleInfo

-- | Update the evaluated module table.
modifyModuleTable :: MonadModuleTable location term value m => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> m ()
modifyModuleTable f = do
  table <- getModuleTable
  putModuleTable $! f table

-- | Update the module load stack.
modifyLoadStack :: MonadModuleTable location term value m => (LoadStack -> LoadStack) -> m ()
modifyLoadStack f = do
  stack <- getLoadStack
  putLoadStack $! f stack


-- | A 'Monad' abstracting jumps in imperative control.
class Monad m => MonadControl term m where
  -- | Allocate a 'Label' for the given @term@.
  --
  --   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
  label :: term -> m Label
  -- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
  goto :: Label -> m term


-- | 'Monad's which can throw exceptions of type @exc v@ which can be resumed with a value of type @v@.
class Monad m => MonadResume exc m where
  throwResumable :: exc v -> m v
  catchResumable :: m v -> (forall v1. exc v1 -> m v) -> m v

instance (Effectful m1, Member (Resumable exc) effects, Monad (m1 effects)) => MonadResume exc (m1 effects) where
  throwResumable = raise . Resumable.throwError
  catchResumable c f = raise (Resumable.catchError (lower c) (lower . f))

class Monad m => MonadExc exc m where
  throwException :: exc -> m v
  catchException :: m v -> (exc -> m v) -> m v

instance (Effectful m1, Member (Exc exc) effects, Monad (m1 effects)) => MonadExc exc (m1 effects) where
  throwException = raise . Exception.throwError
  catchException c f = raise (Exception.catchError (lower c) (lower . f))
