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

-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: (Member (Reader (Environment location)) effects, Member (State (Environment location)) effects) => Name -> Evaluator location value effects (Maybe location)
lookupEnv name = (<|>) <$> (Env.lookup name <$> getEnv) <*> (Env.lookup name <$> defaultEnvironment)

-- | Bind a 'Name' to an 'Address' in the current scope.
bind :: Member (State (Environment location)) effects => Name -> location -> Evaluator location value effects ()
bind name = modifyEnv . Env.insert name

-- | Bind all of the names from an 'Environment' in the current scope.
bindAll :: Member (State (Environment location)) effects => Environment location -> Evaluator location value effects ()
bindAll = foldr ((>>) . uncurry bind) (pure ()) . pairs

-- | Run an action in a new local environment.
locally :: Member (State (Environment location)) effects => Evaluator location value effects a -> Evaluator location value effects a
locally a = do
  modifyEnv Env.push
  a' <- a
  a' <$ modifyEnv Env.pop


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
