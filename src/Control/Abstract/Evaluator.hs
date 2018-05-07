{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Abstract.Evaluator
  ( Evaluator(..)
  -- * State
  , Environment
  , ModuleTable
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
  -- * Module tables
  , getModuleTable
  , putModuleTable
  , modifyModuleTable
  -- * Effects
  , EvalClosure(..)
  , evaluateClosureBody
  , runEvalClosure
  , EvalModule(..)
  , evaluateModule
  , runEvalModule
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
import Control.Monad.Effect.State hiding (runState)
import Data.Abstract.Address
import Data.Abstract.Environment as Env
import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Prelude hiding (fail)
import Prologue

newtype Evaluator location term value effects a = Evaluator { runEvaluator :: Eff.Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Evaluator location term value effects)


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


-- Effects

-- | An effect to evaluate a closure’s body.
data EvalClosure term value resume where
  EvalClosure :: term -> EvalClosure term value value

evaluateClosureBody :: Member (EvalClosure term value) effects => term -> Evaluator location term value effects value
evaluateClosureBody = raise . Eff.send . EvalClosure

runEvalClosure :: (term -> Evaluator location term value effects value) -> Evaluator location term value (EvalClosure term value ': effects) a -> Evaluator location term value effects a
runEvalClosure evalClosure = runEffect (\ (EvalClosure term) yield -> evalClosure term >>= yield)


-- | An effect to evaluate a module.
data EvalModule term value resume where
  EvalModule :: Module term -> EvalModule term value value

evaluateModule :: Member (EvalModule term value) effects => Module term -> Evaluator location term value effects value
evaluateModule = raise . Eff.send . EvalModule

runEvalModule :: (Module term -> Evaluator location term value effects value) -> Evaluator location term value (EvalModule term value ': effects) a -> Evaluator location term value effects a
runEvalModule evalModule = runEffect (\ (EvalModule m) yield -> evalModule m >>= yield)


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
runReturn = runEffect (\ (Return value) _ -> pure value)


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
runLoopControl = runEffect (\ eff _ -> case eff of
  Break    value -> pure value
  Continue value -> pure value)
