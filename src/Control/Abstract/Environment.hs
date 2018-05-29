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
, Env(..)
, EnvironmentError(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Environment as Env
import Data.Abstract.Name
import Prologue

-- | Retrieve the environment.
getEnv :: Member (State (Environment location)) effects => Evaluator location value effects (Environment location)
getEnv = get

-- | Set the environment.
putEnv :: Member (State (Environment location)) effects => Environment location -> Evaluator location value effects ()
putEnv = put

-- | Update the global environment.
modifyEnv :: Member (State (Environment location)) effects => (Environment location -> Environment location) -> Evaluator location value effects ()
modifyEnv = modify'

-- | Sets the environment for the lifetime of the given action.
withEnv :: Member (State (Environment location)) effects => Environment location -> Evaluator location value effects a -> Evaluator location value effects a
withEnv = localState . const


-- | Retrieve the default environment.
defaultEnvironment :: Member (Reader (Environment location)) effects => Evaluator location value effects (Environment location)
defaultEnvironment = ask

-- | Set the default environment for the lifetime of an action.
--   Usually only invoked in a top-level evaluation function.
withDefaultEnvironment :: Member (Reader (Environment location)) effects => Environment location -> Evaluator location value effects a -> Evaluator location value effects a
withDefaultEnvironment e = local (const e)

-- | Obtain an environment that is the composition of the current and default environments.
--   Useful for debugging.
fullEnvironment :: (Member (Reader (Environment location)) effects, Member (State (Environment location)) effects) => Evaluator location value effects (Environment location)
fullEnvironment = mergeEnvs <$> getEnv <*> defaultEnvironment

-- | Run an action with a locally-modified environment.
localEnv :: Member (State (Environment location)) effects => (Environment location -> Environment location) -> Evaluator location value effects a -> Evaluator location value effects a
localEnv f a = do
  modifyEnv (f . Env.push)
  result <- a
  result <$ modifyEnv Env.pop

-- | Run a computation in a new local environment.
localize :: Member (State (Environment location)) effects => Evaluator location value effects a -> Evaluator location value effects a
localize = localEnv id

-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: (Member (Reader (Environment location)) effects, Member (State (Environment location)) effects) => Name -> Evaluator location value effects (Maybe (Address location value))
lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)


data Env location return where
  Lookup :: Name -> Env location (Maybe location)


-- | Errors involving the environment.
data EnvironmentError location return where
  FreeVariable :: Name -> EnvironmentError location location

deriving instance Eq (EnvironmentError location return)
deriving instance Show (EnvironmentError location return)
instance Show1 (EnvironmentError location) where liftShowsPrec _ _ = showsPrec
instance Eq1 (EnvironmentError location) where liftEq _ (FreeVariable n1) (FreeVariable n2) = n1 == n2

freeVariableError :: Member (Resumable (EnvironmentError location)) effects => Name -> Evaluator location value effects location
freeVariableError = throwResumable . FreeVariable

runEnvironmentError :: Effectful (m location value) => m location value (Resumable (EnvironmentError location) ': effects) a -> m location value effects (Either (SomeExc (EnvironmentError location)) a)
runEnvironmentError = runResumable

runEnvironmentErrorWith :: Effectful (m location value) => (forall resume . EnvironmentError location resume -> m location value effects resume) -> m location value (Resumable (EnvironmentError location) ': effects) a -> m location value effects a
runEnvironmentErrorWith = runResumableWith
