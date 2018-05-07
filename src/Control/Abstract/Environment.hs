{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module Control.Abstract.Environment
( Environment
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
, EnvironmentError(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Environment as Env
import Data.Abstract.FreeVariables
import Prologue

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


-- | Errors involving the environment.
data EnvironmentError value return where
  FreeVariable :: Name -> EnvironmentError value value

deriving instance Eq (EnvironmentError value return)
deriving instance Show (EnvironmentError value return)
instance Show1 (EnvironmentError value) where liftShowsPrec _ _ = showsPrec
instance Eq1 (EnvironmentError value) where liftEq _ (FreeVariable n1) (FreeVariable n2) = n1 == n2

freeVariableError :: Member (Resumable (EnvironmentError value)) effects => Name -> Evaluator location term value effects value
freeVariableError = throwResumable . FreeVariable

runEnvironmentError :: Evaluator location term value (Resumable (EnvironmentError value) ': effects) a -> Evaluator location term value effects (Either (SomeExc (EnvironmentError value)) a)
runEnvironmentError = raiseHandler runError

runEnvironmentErrorWith :: (forall resume . EnvironmentError value resume -> Evaluator location term value effects resume) -> Evaluator location term value (Resumable (EnvironmentError value) ': effects) a -> Evaluator location term value effects a
runEnvironmentErrorWith = runResumableWith
