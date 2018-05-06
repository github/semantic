{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Abstract.Evaluator
  ( Evaluator(..)
  -- * State
  , Environment
  , Heap
  , ModuleTable
  , Exports
  , JumpTable
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
  , currentModule
  , currentPackage
  -- * Control
  , label
  , goto
  -- * Effects
  , EvalClosure(..)
  , evaluateClosureBody
  , handleClosuresWith
  , EvalModule(..)
  , evaluateModule
  , handleModulesWith
  , Return(..)
  , earlyReturn
  , catchReturn
  , runReturn
  , LoopControl(..)
  , throwBreak
  , throwContinue
  , catchLoopControl
  , runLoopControl
  , module Control.Effect
  , module Control.Monad.Effect.Fail
  , module Control.Monad.Effect.Fresh
  , module Control.Monad.Effect.NonDet
  , module Control.Monad.Effect.Reader
  , module Control.Monad.Effect.Resumable
  , module Control.Monad.Effect.State
  , Eff.relay
  ) where

import Control.Effect
import qualified Control.Monad.Effect as Eff
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Control.Monad.Effect.Reader hiding (runReader)
import Control.Monad.Effect.Resumable
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
import qualified Data.IntMap as IntMap
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prelude hiding (fail)
import Prologue

newtype Evaluator location term value effects a = Evaluator { runEvaluator :: Eff.Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Evaluator location term value effects)

type JumpTable term = IntMap.IntMap (PackageInfo, ModuleInfo, term)


-- Environment

-- | Retrieve the environment.
getEnv :: Member (State (Environment location value)) effects => Evaluator location term value effects (Environment location value)
getEnv = raise get

-- | Set the environment.
putEnv :: Member (State (Environment location value)) effects => Environment location value -> Evaluator location term value effects ()
putEnv = raise . put

-- | Update the global environment.
modifyEnv :: Member (State (Environment location value)) effects => (Environment location value -> Environment location value) -> Evaluator location term value effects ()
modifyEnv = raise . modify'

-- | Sets the environment for the lifetime of the given action.
withEnv :: Member (State (Environment location value)) effects => Environment location value -> Evaluator location term value effects a -> Evaluator location term value effects a
withEnv = raiseHandler . localState . const


-- | Retrieve the default environment.
defaultEnvironment :: Member (Reader (Environment location value)) effects => Evaluator location term value effects (Environment location value)
defaultEnvironment = raise ask

-- | Set the default environment for the lifetime of an action.
--   Usually only invoked in a top-level evaluation function.
withDefaultEnvironment :: Member (Reader (Environment location value)) effects => Environment location value -> Evaluator location term value effects a -> Evaluator location term value effects a
withDefaultEnvironment e = raiseHandler (local (const e))

-- | Obtain an environment that is the composition of the current and default environments.
--   Useful for debugging.
fullEnvironment :: Members '[Reader (Environment location value), State (Environment location value)] effects => Evaluator location term value effects (Environment location value)
fullEnvironment = mergeEnvs <$> getEnv <*> defaultEnvironment

-- | Run an action with a locally-modified environment.
localEnv :: Member (State (Environment location value)) effects => (Environment location value -> Environment location value) -> Evaluator location term value effects a -> Evaluator location term value effects a
localEnv f a = do
  modifyEnv (f . Env.push)
  result <- a
  result <$ modifyEnv Env.pop

-- | Run a computation in a new local environment.
localize :: Member (State (Environment location value)) effects => Evaluator location term value effects a -> Evaluator location term value effects a
localize = localEnv id

-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: Members '[Reader (Environment location value), State (Environment location value)] effects => Name -> Evaluator location term value effects (Maybe (Address location value))
lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)

-- | Look up a 'Name' in the environment, running an action with the resolved address (if any).
lookupWith :: Members '[Reader (Environment location value), State (Environment location value)] effects => (Address location value -> Evaluator location term value effects a) -> Name -> Evaluator location term value effects (Maybe a)
lookupWith with name = do
  addr <- lookupEnv name
  maybe (pure Nothing) (fmap Just . with) addr


-- Exports

-- | Get the global export state.
getExports :: Member (State (Exports location value)) effects => Evaluator location term value effects (Exports location value)
getExports = raise get

-- | Set the global export state.
putExports :: Member (State (Exports location value)) effects => Exports location value -> Evaluator location term value effects ()
putExports = raise . put

-- | Update the global export state.
modifyExports :: Member (State (Exports location value)) effects => (Exports location value -> Exports location value) -> Evaluator location term value effects ()
modifyExports = raise . modify'

-- | Add an export to the global export state.
addExport :: Member (State (Exports location value)) effects => Name -> Name -> Maybe (Address location value) -> Evaluator location term value effects ()
addExport name alias = modifyExports . Export.insert name alias

-- | Sets the global export state for the lifetime of the given action.
withExports :: Member (State (Exports location value)) effects => Exports location value -> Evaluator location term value effects a -> Evaluator location term value effects a
withExports = raiseHandler . localState . const

-- | Isolate the given action with an empty global environment and exports.
isolate :: Members '[State (Environment location value), State (Exports location value)] effects => Evaluator location term value effects a -> Evaluator location term value effects a
isolate = withEnv lowerBound . withExports lowerBound


-- Heap

-- | Retrieve the heap.
getHeap :: Member (State (Heap location value)) effects => Evaluator location term value effects (Heap location value)
getHeap = raise get

-- | Set the heap.
putHeap :: Member (State (Heap location value)) effects => Heap location value -> Evaluator location term value effects ()
putHeap = raise . put

-- | Update the heap.
modifyHeap :: Member (State (Heap location value)) effects => (Heap location value -> Heap location value) -> Evaluator location term value effects ()
modifyHeap = raise . modify'

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (Member (State (Heap location value)) effects, Ord location) => Address location value -> Evaluator location term value effects (Maybe (Cell location value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Member (State (Heap location value)) effects
          , Ord location
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> Evaluator location term value effects ()
assign address = modifyHeap . heapInsert address


-- Roots

-- | Retrieve the local 'Live' set.
askRoots :: Member (Reader (Live location value)) effects => Evaluator location term value effects (Live location value)
askRoots = raise ask

-- | Run a computation with the given 'Live' set added to the local root set.
extraRoots :: (Member (Reader (Live location value)) effects, Ord location) => Live location value -> Evaluator location term value effects a -> Evaluator location term value effects a
extraRoots roots = raiseHandler (local (<> roots))


-- Configuration

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: Members '[Reader (Live location value), State (Environment location value), State (Heap location value)] effects => term -> Evaluator location term value effects (Configuration location term value)
getConfiguration term = Configuration term <$> askRoots <*> getEnv <*> getHeap


-- Module table

-- | Retrieve the table of evaluated modules.
getModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => Evaluator location term value effects (ModuleTable (Environment location value, value))
getModuleTable = raise get

-- | Set the table of evaluated modules.
putModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => ModuleTable (Environment location value, value) -> Evaluator location term value effects ()
putModuleTable = raise . put

-- | Update the evaluated module table.
modifyModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> Evaluator location term value effects ()
modifyModuleTable = raise . modify'


-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: Member (Reader ModuleInfo) effects => Evaluator location term value effects ModuleInfo
currentModule = raise ask

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: Member (Reader PackageInfo) effects => Evaluator location term value effects PackageInfo
currentPackage = raise ask


-- Control

getJumpTable :: Member (State (JumpTable term)) effects => Evaluator location term vlaue effects (JumpTable term)
getJumpTable = raise get

-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: Members '[Reader ModuleInfo, Reader PackageInfo, State (JumpTable term)] effects => term -> Evaluator location term value effects Label
label term = do
  m <- getJumpTable
  moduleInfo <- currentModule
  packageInfo <- currentPackage
  let i = IntMap.size m
  raise (put (IntMap.insert i (packageInfo, moduleInfo, term) m))
  pure i

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
goto :: Members '[Fail, Reader ModuleInfo, Reader PackageInfo, State (JumpTable term)] effects => Label -> (term -> Evaluator location term value effects a) -> Evaluator location term value effects a
goto label comp = do
  maybeTerm <- IntMap.lookup label <$> getJumpTable
  case maybeTerm of
    Just (packageInfo, moduleInfo, term) -> raiseHandler (local (const packageInfo)) (raiseHandler (local (const moduleInfo)) (comp term))
    Nothing -> raise (fail ("unknown label: " <> show label))


-- Effects

-- | An effect to evaluate a closure’s body.
data EvalClosure term value resume where
  EvalClosure :: term -> EvalClosure term value value

evaluateClosureBody :: Member (EvalClosure term value) effects => term -> Evaluator location term value effects value
evaluateClosureBody = raise . Eff.send . EvalClosure

handleClosuresWith :: (term -> Evaluator location term value effects value) -> Evaluator location term value (EvalClosure term value ': effects) a -> Evaluator location term value effects a
handleClosuresWith evalClosure = raiseHandler (Eff.relay pure (\ (EvalClosure term) yield -> lower (evalClosure term) >>= yield))


-- | An effect to evaluate a module.
data EvalModule term value resume where
  EvalModule :: Module term -> EvalModule term value value

evaluateModule :: Member (EvalModule term value) effects => Module term -> Evaluator location term value effects value
evaluateModule = raise . Eff.send . EvalModule

handleModulesWith :: (Module term -> Evaluator location term value effects value) -> Evaluator location term value (EvalModule term value ': effects) a -> Evaluator location term value effects a
handleModulesWith evalModule = raiseHandler (Eff.relay pure (\ (EvalModule m) yield -> lower (evalModule m) >>= yield))


-- | An effect for explicitly returning out of a function/method body.
data Return value resume where
  Return :: value -> Return value value

deriving instance Eq value => Eq (Return value a)
deriving instance Show value => Show (Return value a)

earlyReturn :: Member (Return value) effects => value -> Evaluator location term value effects value
earlyReturn = raise . Eff.send . Return

catchReturn :: Member (Return value) effects => (forall x . Return value x -> Evaluator location term value effects a) -> Evaluator location term value effects a -> Evaluator location term value effects a
catchReturn handler = raiseHandler (Eff.interpose pure (\ ret _ -> lower (handler ret)))

runReturn :: Evaluator location term value (Return value ': effects) value -> Evaluator location term value effects value
runReturn = raiseHandler (Eff.relay pure (\ (Return value) _ -> pure value))


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl value resume where
  Break    :: value -> LoopControl value value
  Continue :: value -> LoopControl value value

deriving instance Eq value => Eq (LoopControl value a)
deriving instance Show value => Show (LoopControl value a)

throwBreak :: Member (LoopControl value) effects => value -> Evaluator location term value effects value
throwBreak = raise . Eff.send . Break

throwContinue :: Member (LoopControl value) effects => value -> Evaluator location term value effects value
throwContinue = raise . Eff.send . Continue

catchLoopControl :: Member (LoopControl value) effects => Evaluator location term value effects a -> (forall x . LoopControl value x -> Evaluator location term value effects a) -> Evaluator location term value effects a
catchLoopControl action handler = raiseHandler (Eff.interpose pure (\ control _ -> lower (handler control))) action

runLoopControl :: Evaluator location term value (LoopControl value ': effects) value -> Evaluator location term value effects value
runLoopControl = raiseHandler (Eff.relay pure (\ eff _ -> case eff of
  Break    value -> pure value
  Continue value -> pure value))
