{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module Control.Abstract.Environment
( Environment
, getEnv
, putEnv
, withEnv
, withDefaultEnvironment
, lookupEnv
, bind
, bindAll
, locally
, EnvironmentError(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Data.Abstract.Environment as Env
import Data.Abstract.Name
import Prologue

-- | Retrieve the environment.
getEnv :: Member (State (Environment address)) effects => Evaluator address value effects (Environment address)
getEnv = get

-- | Set the environment.
putEnv :: Member (State (Environment address)) effects => Environment address -> Evaluator address value effects ()
putEnv = put

-- | Update the global environment.
modifyEnv :: Member (State (Environment address)) effects => (Environment address -> Environment address) -> Evaluator address value effects ()
modifyEnv = modify'

-- | Sets the environment for the lifetime of the given action.
withEnv :: Member (State (Environment address)) effects => Environment address -> Evaluator address value effects a -> Evaluator address value effects a
withEnv = localState . const


-- | Retrieve the default environment.
defaultEnvironment :: Member (Reader (Environment address)) effects => Evaluator address value effects (Environment address)
defaultEnvironment = ask

-- | Set the default environment for the lifetime of an action.
--   Usually only invoked in a top-level evaluation function.
withDefaultEnvironment :: Member (Reader (Environment address)) effects => Environment address -> Evaluator address value effects a -> Evaluator address value effects a
withDefaultEnvironment e = local (const e)

-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: (Member (Reader (Environment address)) effects, Member (State (Environment address)) effects) => Name -> Evaluator address value effects (Maybe address)
lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)

-- | Bind a 'Name' to an 'Address' in the current scope.
bind :: Member (State (Environment address)) effects => Name -> address -> Evaluator address value effects ()
bind name = modifyEnv . Env.insert name

-- | Bind all of the names from an 'Environment' in the current scope.
bindAll :: Member (State (Environment address)) effects => Environment address -> Evaluator address value effects ()
bindAll = foldr ((>>) . uncurry bind) (pure ()) . pairs

-- | Run an action in a new local environment.
locally :: Member (State (Environment address)) effects => Evaluator address value effects a -> Evaluator address value effects a
locally a = do
  modifyEnv Env.push
  a' <- a
  a' <$ modifyEnv Env.pop


-- | Errors involving the environment.
data EnvironmentError address return where
  FreeVariable :: Name -> EnvironmentError address address

deriving instance Eq (EnvironmentError address return)
deriving instance Show (EnvironmentError address return)
instance Show1 (EnvironmentError address) where liftShowsPrec _ _ = showsPrec
instance Eq1 (EnvironmentError address) where liftEq _ (FreeVariable n1) (FreeVariable n2) = n1 == n2

freeVariableError :: Member (Resumable (EnvironmentError address)) effects => Name -> Evaluator address value effects address
freeVariableError = throwResumable . FreeVariable

runEnvironmentError :: Effectful (m address value) => m address value (Resumable (EnvironmentError address) ': effects) a -> m address value effects (Either (SomeExc (EnvironmentError address)) a)
runEnvironmentError = runResumable

runEnvironmentErrorWith :: Effectful (m address value) => (forall resume . EnvironmentError address resume -> m address value effects resume) -> m address value (Resumable (EnvironmentError address) ': effects) a -> m address value effects a
runEnvironmentErrorWith = runResumableWith
