{-# LANGUAGE FunctionalDependencies, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Control.Abstract.Evaluator
  ( Evaluator
  , MonadEvaluator
  -- * Environment
  , getEnv
  , putEnv
  , modifyEnv
  , withEnv
  , defaultEnvironment
  , withDefaultEnvironment
  , fullEnvironment
  , localEnv
  , localize
  , lookupEnv
  , lookupWith
  -- * Exports
  , getExports
  , putExports
  , modifyExports
  , addExport
  , withExports
  , isolate
  -- * Heap
  , getHeap
  , putHeap
  , modifyHeap
  , lookupHeap
  , assign
  -- * Roots
  , askRoots
  , extraRoots
  -- * Configuration
  , getConfiguration
  -- * Module tables
  , getModuleTable
  , putModuleTable
  , modifyModuleTable
  , askLoadStack
  , localLoadStack
  , currentModule
  , currentPackage
  -- * Control
  , label
  , goto
  -- * Effects
  , EvalClosure(..)
  , evaluateClosureBody
  , EvalModule(..)
  , evaluateModule
  , Return(..)
  , earlyReturn
  , handleReturn
  , LoopControl(..)
  , throwBreak
  , throwContinue
  , catchLoopControl
  -- * Origin
  , askOrigin
  , pushOrigin
  ) where

import Control.Effect
import qualified Control.Monad.Effect as Eff
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Environment as Env
import Data.Abstract.Exports as Export
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Data.Abstract.Package
import Data.Abstract.Origin
import qualified Data.IntMap as IntMap
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prelude hiding (fail)
import Prologue

class Effectful m => Evaluator location term value m | m -> location term value

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class ( Evaluator location term value m
      , Member (Reader (Environment location value)) effects
      , Member (Reader LoadStack) effects
      , Member (Reader (SomeOrigin term)) effects
      , Member (State (Environment location value)) effects
      , Member (State (Heap location value)) effects
      , Member (State (ModuleTable (Environment location value, value))) effects
      , Member (State (Exports location value)) effects
      , Member (State (IntMap.IntMap (SomeOrigin term, term))) effects
      , Monad (m effects)
      )
   => MonadEvaluator location term value effects m


-- Environment

-- | Retrieve the environment.
getEnv :: (Member (State (Environment location value)) effects, Evaluator location term value m) => m effects (Environment location value)
getEnv = raise get

-- | Set the environment.
putEnv :: MonadEvaluator location term value effects m => Environment location value -> m effects ()
putEnv = raise . put

-- | Update the global environment.
modifyEnv :: MonadEvaluator location term value effects m => (Environment location value -> Environment location value) -> m effects ()
modifyEnv = raise . modify'

-- | Sets the environment for the lifetime of the given action.
withEnv :: MonadEvaluator location term value effects m => Environment location value -> m effects a -> m effects a
withEnv = raiseHandler . localState . const


-- | Retrieve the default environment.
defaultEnvironment :: MonadEvaluator location term value effects m => m effects (Environment location value)
defaultEnvironment = raise ask

-- | Set the default environment for the lifetime of an action.
--   Usually only invoked in a top-level evaluation function.
withDefaultEnvironment :: MonadEvaluator location term value effects m => Environment location value -> m effects a -> m effects a
withDefaultEnvironment e = raiseHandler (local (const e))

-- | Obtain an environment that is the composition of the current and default environments.
--   Useful for debugging.
fullEnvironment :: MonadEvaluator location term value effects m => m effects (Environment location value)
fullEnvironment = mergeEnvs <$> getEnv <*> defaultEnvironment

-- | Run an action with a locally-modified environment.
localEnv :: MonadEvaluator location term value effects m => (Environment location value -> Environment location value) -> m effects a -> m effects a
localEnv f a = do
  modifyEnv (f . Env.push)
  result <- a
  result <$ modifyEnv Env.pop

-- | Run a computation in a new local environment.
localize :: MonadEvaluator location term value effects m => m effects a -> m effects a
localize = localEnv id

-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: MonadEvaluator location term value effects m => Name -> m effects (Maybe (Address location value))
lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)

-- | Look up a 'Name' in the environment, running an action with the resolved address (if any).
lookupWith :: MonadEvaluator location term value effects m => (Address location value -> m effects a) -> Name -> m effects (Maybe a)
lookupWith with name = do
  addr <- lookupEnv name
  maybe (pure Nothing) (fmap Just . with) addr


-- Exports

-- | Get the global export state.
getExports :: MonadEvaluator location term value effects m => m effects (Exports location value)
getExports = raise get

-- | Set the global export state.
putExports :: MonadEvaluator location term value effects m => Exports location value -> m effects ()
putExports = raise . put

-- | Update the global export state.
modifyExports :: MonadEvaluator location term value effects m => (Exports location value -> Exports location value) -> m effects ()
modifyExports = raise . modify'

-- | Add an export to the global export state.
addExport :: MonadEvaluator location term value effects m => Name -> Name -> Maybe (Address location value) -> m effects ()
addExport name alias = modifyExports . Export.insert name alias

-- | Sets the global export state for the lifetime of the given action.
withExports :: MonadEvaluator location term value effects m => Exports location value -> m effects a -> m effects a
withExports = raiseHandler . localState . const

-- | Isolate the given action with an empty global environment and exports.
isolate :: MonadEvaluator location term value effects m => m effects a -> m effects a
isolate = withEnv lowerBound . withExports lowerBound


-- Heap

-- | Retrieve the heap.
getHeap :: (Member (State (Heap location value)) effects, Evaluator location term value m) => m effects (Heap location value)
getHeap = raise get

-- | Set the heap.
putHeap :: MonadEvaluator location term value effects m => Heap location value -> m effects ()
putHeap = raise . put

-- | Update the heap.
modifyHeap :: MonadEvaluator location term value effects m => (Heap location value -> Heap location value) -> m effects ()
modifyHeap = raise . modify'

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (MonadEvaluator location term value effects m, Ord location) => Address location value -> m effects (Maybe (Cell location value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord location
          , MonadEvaluator location term value effects m
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> m effects ()
assign address = modifyHeap . heapInsert address


-- Roots

-- | Retrieve the local 'Live' set.
askRoots :: (Effectful m, Member (Reader (Live location value)) effects) => m effects (Live location value)
askRoots = raise ask

-- | Run a computation with the given 'Live' set added to the local root set.
extraRoots :: (Effectful m, Member (Reader (Live location value)) effects, Ord location) => Live location value -> m effects a -> m effects a
extraRoots roots = raiseHandler (local (<> roots))


-- Configuration

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Applicative (m effects), Member (Reader (Live location value)) effects, Member (State (Environment location value)) effects, Member (State (Heap location value)) effects, Evaluator location term value m) => term -> m effects (Configuration location term value)
getConfiguration term = Configuration term <$> askRoots <*> getEnv <*> getHeap


-- Module table

-- | Retrieve the table of evaluated modules.
getModuleTable :: MonadEvaluator location term value effects m => m effects (ModuleTable (Environment location value, value))
getModuleTable = raise get

-- | Set the table of evaluated modules.
putModuleTable :: MonadEvaluator location term value effects m => ModuleTable (Environment location value, value) -> m effects ()
putModuleTable = raise . put

-- | Update the evaluated module table.
modifyModuleTable :: MonadEvaluator location term value effects m => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> m effects ()
modifyModuleTable = raise . modify'


-- | Retrieve the module load stack
askLoadStack :: MonadEvaluator location term value effects m => m effects LoadStack
askLoadStack = raise ask

-- | Locally update the module load stack.
localLoadStack :: MonadEvaluator location term value effects m => (LoadStack -> LoadStack) -> m effects a -> m effects a
localLoadStack = raiseHandler . local


-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: (Effectful m, Member (Reader ModuleInfo) effects) => m effects ModuleInfo
currentModule = raise ask

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: (Effectful m, Member (Reader PackageInfo) effects) => m effects PackageInfo
currentPackage = raise ask


-- Control

-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: MonadEvaluator location term value effects m => term -> m effects Label
label term = do
  m <- raise get
  origin <- askOrigin
  let i = IntMap.size m
  raise (put (IntMap.insert i (origin, term) m))
  pure i

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
goto :: (Recursive term, Member Fail effects, MonadEvaluator location term value effects m) => Label -> (term -> m effects a) -> m effects a
goto label comp = do
  maybeTerm <- IntMap.lookup label <$> raise get
  case maybeTerm of
    Just (origin, term) -> pushOrigin (origin <> termOrigin term) (comp term)
    Nothing -> raise (fail ("unknown label: " <> show label))


-- Effects

-- | An effect to evaluate a closure’s body.
data EvalClosure term value resume where
  EvalClosure :: term -> EvalClosure term value value

evaluateClosureBody :: (Effectful m, Member (EvalClosure term value) effects) => term -> m effects value
evaluateClosureBody = raise . Eff.send . EvalClosure


-- | An effect to evaluate a module.
data EvalModule term value resume where
  EvalModule :: Module term -> EvalModule term value value

evaluateModule :: (Effectful m, Member (EvalModule term value) effects) => Module term -> m effects value
evaluateModule = raise . Eff.send . EvalModule


-- | An effect for explicitly returning out of a function/method body.
data Return value resume where
  Return :: value -> Return value value

deriving instance Eq value => Eq (Return value a)
deriving instance Show value => Show (Return value a)

earlyReturn :: (Effectful m, Member (Return value) effects) => value -> m effects value
earlyReturn = raise . Eff.send . Return

handleReturn :: (Effectful m, Member (Return value) effects) => (forall x . Return value x -> m effects a) -> m effects a -> m effects a
handleReturn handler action = raiseHandler (Eff.interpose pure (\ ret _ -> lower (handler ret))) action


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl value resume where
  Break :: value -> LoopControl value value
  Continue :: LoopControl value value

deriving instance Eq value => Eq (LoopControl value a)
deriving instance Show value => Show (LoopControl value a)

throwBreak :: (Effectful m, Member (LoopControl value) effects) => value -> m effects value
throwBreak = raise . Eff.send . Break

throwContinue :: (Effectful m, Member (LoopControl value) effects) => m effects value
throwContinue = raise (Eff.send Continue)

catchLoopControl :: (Effectful m, Member (LoopControl value) effects) => m effects a -> (forall x . LoopControl value x -> m effects a) -> m effects a
catchLoopControl action handler = raiseHandler (Eff.interpose pure (\ control _ -> lower (handler control))) action


-- | Retrieve the current 'SomeOrigin'.
askOrigin :: MonadEvaluator location term value effects m => m effects (SomeOrigin term)
askOrigin = raise ask

-- | Push a 'SomeOrigin' onto the stack. This should be used to contextualize execution with information about the originating term, module, or package.
pushOrigin :: ( Effectful m
              , Member (Reader (SomeOrigin term)) effects
              )
           => SomeOrigin term
           -> m effects a
           -> m effects a
pushOrigin o = raiseHandler (local (<> o))
