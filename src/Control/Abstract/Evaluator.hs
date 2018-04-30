{-# LANGUAGE ConstrainedClassMethods, FunctionalDependencies, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Control.Abstract.Evaluator
  ( MonadEvaluator
  -- State
  , EvaluatorState(..)
  -- Environment
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
  -- Exports
  , getExports
  , putExports
  , modifyExports
  , addExport
  , withExports
  -- Heap
  , getHeap
  , putHeap
  , modifyHeap
  , lookupHeap
  , assign
  -- Roots
  , askRoots
  , extraRoots
  -- Configuration
  , getConfiguration
  -- Module tables
  , getModuleTable
  , putModuleTable
  , modifyModuleTable
  , askModuleTable
  , localModuleTable
  , getLoadStack
  , putLoadStack
  , modifyLoadStack
  , currentModule
  , currentPackage
  -- Control
  , label
  , goto
  -- Effects
  , EvalClosure(..)
  , evaluateClosureBody
  , Return(..)
  , earlyReturn
  , catchReturn
  , LoopControl(..)
  , throwBreak
  , throwContinue
  , catchLoopControl
  -- | Origin
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
import Lens.Micro
import Prelude hiding (fail)
import Prologue

-- | A 'Monad' providing the basic essentials for evaluation.
--
--   These presently include:
--   - environments binding names to addresses
--   - a heap mapping addresses to (possibly sets of) values
--   - tables of modules available for import
class ( Effectful m
      , Member (Reader (Environment location value)) effects
      , Member (Reader (ModuleTable [Module term])) effects
      , Member (Reader (SomeOrigin term)) effects
      , Member (State (EvaluatorState location term value)) effects
      , Monad (m effects)
      )
   => MonadEvaluator location term value effects m | m effects -> location term value


-- State

data EvaluatorState location term value = EvaluatorState
  { environment :: Environment location value
  , heap        :: Heap location value
  , modules     :: ModuleTable (Environment location value, value)
  , loadStack   :: LoadStack
  , exports     :: Exports location value
  , jumps       :: IntMap.IntMap (SomeOrigin term, term)
  }

deriving instance (Eq (Cell location value), Eq location, Eq term, Eq value, Eq (Base term ())) => Eq (EvaluatorState location term value)
deriving instance (Ord (Cell location value), Ord location, Ord term, Ord value, Ord (Base term ())) => Ord (EvaluatorState location term value)
deriving instance (Show (Cell location value), Show location, Show term, Show value, Show (Base term ())) => Show (EvaluatorState location term value)

instance Lower (EvaluatorState location term value) where
  lowerBound = EvaluatorState lowerBound lowerBound lowerBound lowerBound lowerBound lowerBound


-- Lenses

_environment :: Lens' (EvaluatorState location term value) (Environment location value)
_environment = lens environment (\ s e -> s {environment = e})

_heap :: Lens' (EvaluatorState location term value) (Heap location value)
_heap = lens heap (\ s h -> s {heap = h})

_modules :: Lens' (EvaluatorState location term value) (ModuleTable (Environment location value, value))
_modules = lens modules (\ s m -> s {modules = m})

_loadStack :: Lens' (EvaluatorState location term value) LoadStack
_loadStack = lens loadStack (\ s l -> s {loadStack = l})

_exports :: Lens' (EvaluatorState location term value) (Exports location value)
_exports = lens exports (\ s e -> s {exports = e})

_jumps :: Lens' (EvaluatorState location term value) (IntMap.IntMap (SomeOrigin term, term))
_jumps = lens jumps (\ s j -> s {jumps = j})


(.=) :: MonadEvaluator location term value effects m => ASetter (EvaluatorState location term value) (EvaluatorState location term value) a b -> b -> m effects ()
lens .= val = raise (modify' (lens .~ val))

view :: MonadEvaluator location term value effects m => Getting a (EvaluatorState location term value) a -> m effects a
view lens = raise (gets (^. lens))

localEvaluatorState :: MonadEvaluator location term value effects m => Lens' (EvaluatorState location term value) prj -> (prj -> prj) -> m effects a -> m effects a
localEvaluatorState lens f action = do
  original <- view lens
  lens .= f original
  v <- action
  v <$ lens .= original


-- Environment

-- | Retrieve the environment.
getEnv :: MonadEvaluator location term value effects m => m effects (Environment location value)
getEnv = view _environment

-- | Set the environment.
putEnv :: MonadEvaluator location term value effects m => Environment location value -> m effects ()
putEnv = (_environment .=)

-- | Update the global environment.
modifyEnv :: MonadEvaluator location term value effects m => (Environment location value -> Environment location value) -> m effects ()
modifyEnv f = do
  env <- getEnv
  putEnv $! f env

-- | Sets the environment for the lifetime of the given action.
withEnv :: MonadEvaluator location term value effects m => Environment location value -> m effects a -> m effects a
withEnv s = localEvaluatorState _environment (const s)


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
getExports = view _exports

-- | Set the global export state.
putExports :: MonadEvaluator location term value effects m => Exports location value -> m effects ()
putExports = (_exports .=)

-- | Update the global export state.
modifyExports :: MonadEvaluator location term value effects m => (Exports location value -> Exports location value) -> m effects ()
modifyExports f = do
  exports <- getExports
  putExports $! f exports

-- | Add an export to the global export state.
addExport :: MonadEvaluator location term value effects m => Name -> Name -> Maybe (Address location value) -> m effects ()
addExport name alias = modifyExports . Export.insert name alias

-- | Sets the global export state for the lifetime of the given action.
withExports :: MonadEvaluator location term value effects m => Exports location value -> m effects a -> m effects a
withExports s = localEvaluatorState _exports (const s)


-- Heap

-- | Retrieve the heap.
getHeap :: MonadEvaluator location term value effects m => m effects (Heap location value)
getHeap = view _heap

-- | Set the heap.
putHeap :: MonadEvaluator location term value effects m => Heap location value -> m effects ()
putHeap = (_heap .=)

-- | Update the heap.
modifyHeap :: MonadEvaluator location term value effects m => (Heap location value -> Heap location value) -> m effects ()
modifyHeap f = do
  s <- getHeap
  putHeap $! f s

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
getConfiguration :: (Member (Reader (Live location value)) effects, MonadEvaluator location term value effects m) => term -> m effects (Configuration location term value)
getConfiguration term = Configuration term <$> askRoots <*> getEnv <*> getHeap


-- Module table

-- | Retrieve the table of evaluated modules.
getModuleTable :: MonadEvaluator location term value effects m => m effects (ModuleTable (Environment location value, value))
getModuleTable = view _modules

-- | Set the table of evaluated modules.
putModuleTable :: MonadEvaluator location term value effects m => ModuleTable (Environment location value, value) -> m effects ()
putModuleTable = (_modules .=)

-- | Update the evaluated module table.
modifyModuleTable :: MonadEvaluator location term value effects m => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> m effects ()
modifyModuleTable f = do
  table <- getModuleTable
  putModuleTable $! f table


-- | Retrieve the table of unevaluated modules.
askModuleTable :: MonadEvaluator location term value effects m => m effects (ModuleTable [Module term])
askModuleTable = raise ask

-- | Run an action with a locally-modified table of unevaluated modules.
localModuleTable :: MonadEvaluator location term value effects m => (ModuleTable [Module term] -> ModuleTable [Module term]) -> m effects a -> m effects a
localModuleTable f = raiseHandler (local f)


-- | Retrieve the module load stack
getLoadStack :: MonadEvaluator location term value effects m => m effects LoadStack
getLoadStack = view _loadStack

-- | Set the module load stack
putLoadStack :: MonadEvaluator location term value effects m => LoadStack -> m effects ()
putLoadStack = (_loadStack .=)

-- | Update the module load stack.
modifyLoadStack :: MonadEvaluator location term value effects m => (LoadStack -> LoadStack) -> m effects ()
modifyLoadStack f = do
  stack <- getLoadStack
  putLoadStack $! f stack


-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: forall location term value effects m . (Member Fail effects, MonadEvaluator location term value effects m) => m effects ModuleInfo
currentModule = do
  o <- raise ask
  maybeM (raise (fail "unable to get currentModule")) $ withSomeOrigin (originModule @term) o

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: forall location term value effects m . (Member Fail effects, MonadEvaluator location term value effects m) => m effects PackageInfo
currentPackage = do
  o <- raise ask
  maybeM (raise (fail "unable to get currentPackage")) $ withSomeOrigin (originPackage @term) o


-- Control

-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: MonadEvaluator location term value effects m => term -> m effects Label
label term = do
  m <- view _jumps
  origin <- raise ask
  let i = IntMap.size m
  _jumps .= IntMap.insert i (origin, term) m
  pure i

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
goto :: (Recursive term, Member Fail effects, MonadEvaluator location term value effects m) => Label -> (term -> m effects a) -> m effects a
goto label comp = do
  maybeTerm <- IntMap.lookup label <$> view _jumps
  case maybeTerm of
    Just (origin, term) -> pushOrigin (origin <> termOrigin term) (comp term)
    Nothing -> raise (fail ("unknown label: " <> show label))


-- Effects

-- | An effect to evaluate a closure’s body.
data EvalClosure term value resume where
  EvalClosure :: term -> EvalClosure term value value

evaluateClosureBody :: (Effectful m, Member (EvalClosure term value) effects) => term -> m effects value
evaluateClosureBody = raise . Eff.send . EvalClosure


-- | An effect for explicitly returning out of a function/method body.
data Return value resume where
  Return :: value -> Return value value

deriving instance Eq value => Eq (Return value a)
deriving instance Show value => Show (Return value a)

earlyReturn :: (Effectful m, Member (Return value) effects) => value -> m effects value
earlyReturn = raise . Eff.send . Return

catchReturn :: (Effectful m, Member (Return value) effects) => m effects a -> (forall x . Return value x -> m effects a) -> m effects a
catchReturn action handler = raiseHandler (Eff.interpose pure (\ ret _ -> lower (handler ret))) action


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


-- | Push a 'SomeOrigin' onto the stack. This should be used to contextualize execution with information about the originating term, module, or package.
pushOrigin :: ( Effectful m
              , Member (Reader (SomeOrigin term)) effects
              )
           => SomeOrigin term
           -> m effects a
           -> m effects a
pushOrigin o = raiseHandler (local (<> o))
