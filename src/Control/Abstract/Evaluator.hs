{-# LANGUAGE ConstrainedClassMethods, FunctionalDependencies, RankNTypes, TypeFamilies, UndecidableInstances #-}
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
  , throwResumable
  , throwException
  , catchException
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
import Prologue

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class ( Effectful m
      , MonadControl term effects m
      , MonadEnvironment location value effects m
      , MonadModuleTable location term value effects m
      , MonadHeap location value effects m
      )
   => MonadEvaluator location term value (effects :: [* -> *]) m | m effects -> location term value where
  -- | Get the current 'Configuration' with a passed-in term.
  getConfiguration :: Ord location => term -> m effects (Configuration location term value)

-- | A 'Monad' abstracting local and global environments.
class Monad (m effects) => MonadEnvironment location value (effects :: [* -> *]) m | m effects -> value, m -> location where
  -- | Retrieve the environment.
  getEnv :: m effects (Environment location value)
  -- | Set the environment.
  putEnv :: Environment location value -> m effects ()
  -- | Sets the environment for the lifetime of the given action.
  withEnv :: Environment location value -> m effects a -> m effects a

  -- | Retrieve the default environment.
  defaultEnvironment :: m effects (Environment location value)

  -- | Set the default environment for the lifetime of an action.
  --   Usually only invoked in a top-level evaluation function.
  withDefaultEnvironment :: Environment location value -> m effects a -> m effects a

  -- | Get the global export state.
  getExports :: m effects (Exports location value)
  -- | Set the global export state.
  putExports :: Exports location value -> m effects ()
  -- | Sets the global export state for the lifetime of the given action.
  withExports :: Exports location value -> m effects a -> m effects a

  -- | Run an action with a locally-modified environment.
  localEnv :: (Environment location value -> Environment location value) -> m effects a -> m effects a

  -- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
  lookupEnv :: Name -> m effects (Maybe (Address location value))
  lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)

  -- | Look up a 'Name' in the environment, running an action with the resolved address (if any).
  lookupWith :: (Address location value -> m effects a) -> Name -> m effects (Maybe a)
  lookupWith with name = do
    addr <- lookupEnv name
    maybe (pure Nothing) (fmap Just . with) addr

-- | Run a computation in a new local environment.
localize :: MonadEnvironment location value effects m => m effects a -> m effects a
localize = localEnv id

-- | Update the global environment.
modifyEnv :: MonadEnvironment location value effects m => (Environment location value -> Environment location value) -> m effects ()
modifyEnv f = do
  env <- getEnv
  putEnv $! f env

-- | Update the global export state.
modifyExports :: MonadEnvironment location value effects m => (Exports location value -> Exports location value) -> m effects ()
modifyExports f = do
  exports <- getExports
  putExports $! f exports

-- | Add an export to the global export state.
addExport :: MonadEnvironment location value effects m => Name -> Name -> Maybe (Address location value) -> m effects ()
addExport name alias = modifyExports . Export.insert name alias

-- | Obtain an environment that is the composition of the current and default environments.
--   Useful for debugging.
fullEnvironment :: MonadEnvironment location value effects m => m effects (Environment location value)
fullEnvironment = mappend <$> getEnv <*> defaultEnvironment

-- | A 'Monad' abstracting a heap of values.
class Monad (m effects) => MonadHeap location value (effects :: [* -> *]) m | m effects -> location value where
  -- | Retrieve the heap.
  getHeap :: m effects (Heap location value)
  -- | Set the heap.
  putHeap :: Heap location value -> m effects ()

-- | Update the heap.
modifyHeap :: MonadHeap location value effects m => (Heap location value -> Heap location value) -> m effects ()
modifyHeap f = do
  s <- getHeap
  putHeap $! f s

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (MonadHeap location value effects m, Ord location) => Address location value -> m effects (Maybe (Cell location value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord location
          , MonadHeap location value effects m
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> m effects ()
assign address = modifyHeap . heapInsert address


-- | A 'Monad' abstracting tables of modules available for import.
class Monad (m effects) => MonadModuleTable location term value (effects :: [* -> *]) m | m effects -> location term value where
  -- | Retrieve the table of evaluated modules.
  getModuleTable :: m effects (ModuleTable (Environment location value, value))
  -- | Set the table of evaluated modules.
  putModuleTable :: ModuleTable (Environment location value, value) -> m effects ()

  -- | Retrieve the table of unevaluated modules.
  askModuleTable :: m effects (ModuleTable [Module term])
  -- | Run an action with a locally-modified table of unevaluated modules.
  localModuleTable :: (ModuleTable [Module term] -> ModuleTable [Module term]) -> m effects a -> m effects a

  -- | Retrieve the module load stack
  getLoadStack :: m effects LoadStack
  -- | Set the module load stack
  putLoadStack :: LoadStack -> m effects ()

  -- | Get the currently evaluating 'ModuleInfo'.
  currentModule :: m effects ModuleInfo

-- | Update the evaluated module table.
modifyModuleTable :: MonadModuleTable location term value effects m => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> m effects ()
modifyModuleTable f = do
  table <- getModuleTable
  putModuleTable $! f table

-- | Update the module load stack.
modifyLoadStack :: MonadModuleTable location term value effects m => (LoadStack -> LoadStack) -> m effects ()
modifyLoadStack f = do
  stack <- getLoadStack
  putLoadStack $! f stack


-- | A 'Monad' abstracting jumps in imperative control.
class Monad (m effects) => MonadControl term (effects :: [* -> *]) m where
  -- | Allocate a 'Label' for the given @term@.
  --
  --   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
  label :: term -> m effects Label
  -- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
  goto :: Label -> m effects term


throwResumable :: (Member (Resumable exc) effects, Effectful m) => exc v -> m effects v
throwResumable = raise . Resumable.throwError

throwException :: (Member (Exc exc) effects, Effectful m) => exc -> m effects a
throwException = raise . Exception.throwError

catchException :: (Member (Exc exc) effects, Effectful m) => m effects v -> (exc -> m effects v) -> m effects v
catchException action handler = raise (lower action `Exception.catchError` (lower . handler))
