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
, runEnvState
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
getEnv :: Member (Env address) effects => Evaluator address value effects (Environment address)
getEnv = send GetEnv

-- | Set the environment.
putEnv :: Member (Env address) effects => Environment address -> Evaluator address value effects ()
putEnv = send . PutEnv

-- | Sets the environment for the lifetime of the given action.
withEnv :: Member (Env address) effects => Environment address -> Evaluator address value effects a -> Evaluator address value effects a
withEnv env m = do
  oldEnv <- getEnv
  putEnv env
  result <- m
  putEnv oldEnv
  pure result


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
  Lookup :: Name                -> Env address (Maybe address)
  Bind   :: Name -> address     -> Env address ()
  Close  :: Set Name            -> Env address (Environment address)
  Push   ::                        Env address ()
  Pop    ::                        Env address ()
  GetEnv ::                        Env address (Environment address)
  PutEnv :: Environment address -> Env address ()

handleEnv :: forall address effects value result
           . Member (State (Environment address)) effects
          => Env address result
          -> Evaluator address value effects result
handleEnv = \case
  Lookup name -> Env.lookup name <$> get
  Bind name addr -> modify (Env.insert name addr)
  Close names -> Env.intersect names <$> get
  Push -> modify (Env.push @address)
  Pop -> modify (Env.pop @address)
  GetEnv -> get
  PutEnv e -> put e

runEnv :: Member (State (Environment address)) effects
       => Evaluator address value (Env address ': effects) a
       -> Evaluator address value effects a
runEnv = interpret handleEnv

reinterpretEnv :: Evaluator address value (Env address ': effects) a
               -> Evaluator address value (State (Environment address) ': effects) a
reinterpretEnv = reinterpret handleEnv

runEnvState :: forall address value effects a
            .  Environment address
            -> Evaluator address value (Env address ': effects) a
            -> Evaluator address value effects (a, Environment address)
runEnvState initial = relayState initial (\ s a -> pure (a, s)) $ \ s eff yield -> case eff of
  Lookup name -> yield s (Env.lookup name s)
  Bind name addr -> yield (Env.insert name addr s) ()
  Close names -> yield s (Env.intersect names s)
  Push -> yield (Env.push @address s) ()
  Pop -> yield (Env.pop @address s) ()
  GetEnv -> yield s s
  PutEnv e -> yield e ()


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
