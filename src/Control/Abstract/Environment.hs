{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Environment
( Environment
, Exports
, getEnv
, export
, lookupEnv
, bind
, bindAll
, locally
, close
, Env(..)
, runEnv
, EnvironmentError(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Data.Abstract.Environment (Environment)
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Exports as Exports
import Data.Abstract.Name
import Data.Semilattice.Lower
import Prologue

-- | Retrieve the environment.
getEnv :: Member (Env address) effects => Evaluator address value effects (Environment address)
getEnv = send GetEnv

-- | Add an export to the global export state.
export :: Member (Env address) effects => Name -> Name -> Maybe address -> Evaluator address value effects ()
export name alias addr = send (Export name alias addr)


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
  Lookup :: Name            -> Env address (Maybe address)
  Bind   :: Name -> address -> Env address ()
  Close  :: Set Name        -> Env address (Environment address)
  Push   ::                    Env address ()
  Pop    ::                    Env address ()
  GetEnv ::                    Env address (Environment address)
  Export :: Name -> Name -> Maybe address -> Env address ()

handleEnv :: forall address effects value result
          .  ( Member (State (Environment address)) effects
             , Member (State (Exports address)) effects
             )
          => Env address result
          -> Evaluator address value effects result
handleEnv = \case
  Lookup name -> Env.lookup name <$> get
  Bind name addr -> modify (Env.insert name addr)
  Close names -> Env.intersect names <$> get
  Push -> modify (Env.push @address)
  Pop -> modify (Env.pop @address)
  GetEnv -> get
  Export name alias addr -> modify (Exports.insert name alias addr)

runEnv :: Environment address
       -> Evaluator address value (Env address ': effects) a
       -> Evaluator address value effects (a, Environment address)
runEnv initial = fmap (uncurry filterEnv) . runState lowerBound . runState initial . reinterpret2 handleEnv
  where -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
        filterEnv (a, env) ports
          | Exports.null ports = (a, env)
          | otherwise          = (a, Exports.toEnvironment ports `Env.mergeEnvs` Env.overwrite (Exports.aliases ports) env)


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
