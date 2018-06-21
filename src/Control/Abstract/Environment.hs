{-# LANGUAGE GADTs, KindSignatures, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
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
-- * Effects
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


-- Effects

data Env address (m :: * -> *) return where
  Lookup :: Name            -> Env address m (Maybe address)
  Bind   :: Name -> address -> Env address m ()
  Close  :: Set Name        -> Env address m (Environment address)
  Push   ::                    Env address m ()
  Pop    ::                    Env address m ()
  GetEnv ::                    Env address m (Environment address)
  Export :: Name -> Name -> Maybe address -> Env address m ()

instance Effect (Env address) where
  handleState c dist (Request (Lookup name) k) = Request (Lookup name) (dist . (<$ c) . k)
  handleState c dist (Request (Bind name addr) k) = Request (Bind name addr) (dist . (<$ c) . k)
  handleState c dist (Request (Close names) k) = Request (Close names) (dist . (<$ c) . k)
  handleState c dist (Request Push k) = Request Push (dist . (<$ c) . k)
  handleState c dist (Request Pop k) = Request Pop (dist . (<$ c) . k)
  handleState c dist (Request GetEnv k) = Request GetEnv (dist . (<$ c) . k)
  handleState c dist (Request (Export name alias addr) k) = Request (Export name alias addr) (dist . (<$ c) . k)

runEnv :: forall address value effects a
       .  Effects effects
       => Environment address
       -> Evaluator address value (Env address ': effects) a
       -> Evaluator address value effects (Environment address, a)
runEnv initial = fmap (filterEnv . fmap (first Env.head)) . runState lowerBound . runState (Env.push initial) . reinterpret2 (\case
  Lookup name -> Env.lookup name <$> get
  Bind name addr -> modify (Env.insert name addr)
  Close names -> Env.intersect names <$> get
  Push -> modify (Env.push @address)
  Pop -> modify (Env.pop @address)
  GetEnv -> get
  Export name alias addr -> modify (Exports.insert name alias addr))
  where -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
        filterEnv (ports, (env, a))
          | Exports.null ports = (env, a)
          | otherwise          = (Exports.toEnvironment ports `Env.mergeEnvs` Env.overwrite (Exports.aliases ports) env, a)


-- | Errors involving the environment.
data EnvironmentError address return where
  FreeVariable :: Name -> EnvironmentError address address

deriving instance Eq (EnvironmentError address return)
deriving instance Show (EnvironmentError address return)
instance Show1 (EnvironmentError address) where liftShowsPrec _ _ = showsPrec
instance Eq1 (EnvironmentError address) where liftEq _ (FreeVariable n1) (FreeVariable n2) = n1 == n2

freeVariableError :: Member (Resumable (EnvironmentError address)) effects => Name -> Evaluator address value effects address
freeVariableError = throwResumable . FreeVariable

runEnvironmentError :: (Effectful (m address value), Effects effects) => m address value (Resumable (EnvironmentError address) ': effects) a -> m address value effects (Either (SomeExc (EnvironmentError address)) a)
runEnvironmentError = runResumable

runEnvironmentErrorWith :: (Effectful (m address value), Effects effects) => (forall resume . EnvironmentError address resume -> m address value effects resume) -> m address value (Resumable (EnvironmentError address) ': effects) a -> m address value effects a
runEnvironmentErrorWith = runResumableWith
