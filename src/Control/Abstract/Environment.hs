{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Environment
( Environment
, getEnv
, putEnv
, modifyEnv
, withEnv
, localEnv
, locally
, lookupEnv
, bind
, Env(..)
, runEnv
, reinterpretEnv
, EnvironmentError(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Environment (Environment)
import qualified Data.Abstract.Environment as Env
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


-- | Run an action with a locally-modified environment.
localEnv :: Member (State (Environment location)) effects => (Environment location -> Environment location) -> Evaluator location value effects a -> Evaluator location value effects a
localEnv f a = do
  modifyEnv (f . Env.push)
  result <- a
  result <$ modifyEnv Env.pop

locally :: forall location value effects a . Member (Env location) effects => Evaluator location value effects a -> Evaluator location value effects a
locally a = do
  send (Push @location)
  a' <- a
  a' <$ send (Pop @location)

-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: Member (Env location) effects => Name -> Evaluator location value effects (Maybe (Address location value))
lookupEnv name = fmap Address <$> send (Lookup name)

-- | Bind a 'Name' to an 'Address' in the environment.
bind :: Member (Env location) effects => Name -> Address location value -> Evaluator location value effects ()
bind name addr = send (Bind name (unAddress addr))


data Env location return where
  Lookup :: Name             -> Env location (Maybe location)
  Bind   :: Name -> location -> Env location ()
  Push   ::                     Env location ()
  Pop    ::                     Env location ()


runEnv :: Member (State (Environment location)) effects => Environment location -> Evaluator location value (Env location ': effects) a -> Evaluator location value effects a
runEnv defaultEnvironment = interpret $ \case
  Lookup name -> maybe (Env.lookup name defaultEnvironment) Just . Env.lookup name <$> getEnv
  Bind name addr -> modifyEnv (Env.insert name addr)
  Push -> modifyEnv Env.push
  Pop -> modifyEnv Env.pop

reinterpretEnv :: Environment location -> Evaluator location value (Env location ': effects) a -> Evaluator location value (State (Environment location) ': effects) a
reinterpretEnv defaultEnvironment = reinterpret $ \case
  Lookup name -> maybe (Env.lookup name defaultEnvironment) Just . Env.lookup name <$> getEnv
  Bind name addr -> modifyEnv (Env.insert name addr)
  Push -> modifyEnv Env.push
  Pop -> modifyEnv Env.pop


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
