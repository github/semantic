{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Environment
( Environment
, getEnv
, putEnv
, withEnv
, lookupEnv
, bind
, bindAll
, locally
, close
, Env(..)
, runEnv
, reinterpretEnv
, EnvironmentError(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Data.Abstract.Environment (Environment)
import qualified Data.Abstract.Environment as Env
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


-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: Member (Env address) effects => Name -> Evaluator address value effects (Maybe address)
lookupEnv name = send (Lookup name)

-- | Bind a 'Name' to an address in the current scope.
bind :: Member (Env address) effects => Name -> address -> Evaluator address value effects ()
bind name addr = send (Bind name addr)

-- | Bind all of the names from an 'Environment' in the current scope.
bindAll :: Member (Env address) effects => Environment address -> Evaluator address value effects ()
bindAll = foldr ((>>) . uncurry bind) (pure ()) . Env.pairs

-- | Run an action in a new local scope.
locally :: forall address value effects a . Member (Env address) effects => Evaluator address value effects a -> Evaluator address value effects a
locally a = do
  send (Push @address)
  a' <- a
  a' <$ send (Pop @address)

close :: Member (Env address) effects => Set Name -> Evaluator address value effects (Environment address)
close = send . Close


data Env address return where
  Lookup :: Name             -> Env address (Maybe address)
  Bind   :: Name -> address -> Env address ()
  Close  :: Set Name         -> Env address (Environment address)
  Push   ::                     Env address ()
  Pop    ::                     Env address ()

handleEnv :: Member (State (Environment address)) effects => Environment address -> Env address result -> Evaluator address value effects result
handleEnv defaultEnvironment = \case
  Lookup name -> maybe (Env.lookup name defaultEnvironment) Just . Env.lookup name <$> getEnv
  Bind name addr -> modifyEnv (Env.insert name addr)
  Close names -> Env.intersect names <$> getEnv
  Push -> modifyEnv Env.push
  Pop -> modifyEnv Env.pop

runEnv :: Member (State (Environment address)) effects => Environment address -> Evaluator address value (Env address ': effects) a -> Evaluator address value effects a
runEnv defaultEnvironment = interpret (handleEnv defaultEnvironment)

reinterpretEnv :: Environment address -> Evaluator address value (Env address ': effects) a -> Evaluator address value (State (Environment address) ': effects) a
reinterpretEnv defaultEnvironment = reinterpret (handleEnv defaultEnvironment)


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
