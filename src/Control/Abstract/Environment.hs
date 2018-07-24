{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Environment
( Environment
, Exports
, getCtx
, putCtx
, getEnv
, withEnv
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
import Data.Abstract.Environment (Bindings, Environment, EvalContext(..))
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Exports as Exports
import Data.Abstract.Name
import Prologue

-- | Retrieve the current execution context
getCtx :: Member (Env address) effects => Evaluator address value effects (EvalContext address)
getCtx = send GetCtx

-- | Retrieve the current environment
getEnv :: Member (Env address) effects => Evaluator address value effects (Environment address)
getEnv = ctxEnvironment <$> getCtx

-- | Replace the execution context. This is only for use in Analysis.Abstract.Caching.
putCtx :: Member (Env address) effects => EvalContext address -> Evaluator address value effects ()
putCtx = send . PutCtx

withCtx :: Member (Env address) effects
        => EvalContext address
        -> Evaluator address value effects a
        -> Evaluator address value effects a
withCtx ctx comp = do
  oldCtx <- getCtx
  putCtx ctx
  value <- comp
  putCtx oldCtx
  pure value

-- | Replace the environment for a computation
withEnv :: Member (Env address) effects
        => Environment address
        -> Evaluator address value effects a
        -> Evaluator address value effects a
withEnv env = withCtx (EvalContext env)

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
bindAll :: Member (Env address) effects => Bindings address -> Evaluator address value effects ()
bindAll = foldr ((>>) . uncurry bind) (pure ()) . Env.pairs

-- | Run an action in a new local scope.
locally :: forall address value effects a . Member (Env address) effects => Evaluator address value effects a -> Evaluator address value effects a
locally = send . Locally @_ @_ @address . lowerEff

close :: Member (Env address) effects => Set Name -> Evaluator address value effects (Environment address)
close = send . Close


-- Effects

data Env address m return where
  Lookup :: Name            -> Env address m (Maybe address)
  Bind   :: Name -> address -> Env address m ()
  Close  :: Set Name        -> Env address m (Environment address)
  Locally :: m a            -> Env address m a
  GetCtx ::                    Env address m (EvalContext address)
  PutCtx :: EvalContext address -> Env address m ()
  Export :: Name -> Name -> Maybe address -> Env address m ()

instance PureEffect (Env address)
instance Effect (Env address) where
  handleState c dist (Request (Lookup name) k) = Request (Lookup name) (dist . (<$ c) . k)
  handleState c dist (Request (Bind name addr) k) = Request (Bind name addr) (dist . (<$ c) . k)
  handleState c dist (Request (Close names) k) = Request (Close names) (dist . (<$ c) . k)
  handleState c dist (Request (Locally action) k) = Request (Locally (dist (action <$ c))) (dist . fmap k)
  handleState c dist (Request GetCtx k) = Request GetCtx (dist . (<$ c) . k)
  handleState c dist (Request (PutCtx e) k) = Request (PutCtx e) (dist . (<$ c) . k)
  handleState c dist (Request (Export name alias addr) k) = Request (Export name alias addr) (dist . (<$ c) . k)

-- | Runs a computation in the context of an existing environment.
--   New bindings created in the computation are returned.
runEnv :: Effects effects
       => Environment address
       -> Evaluator address value (Env address ': effects) a
       -> Evaluator address value effects (Bindings address, a)
runEnv initial = fmap (filterEnv . fmap (first (Env.head . ctxEnvironment))) . runState lowerBound . runState (EvalContext (Env.push initial)) . reinterpret2 handleEnv
  where -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
  filterEnv (ports, (binds, a))
          | Exports.null ports = (binds, a)
          | otherwise          = (Exports.toBindings ports <> Env.aliasBindings (Exports.aliases ports) binds, a)

handleEnv :: forall address value effects a . Effects effects => Env address (Eff (Env address ': effects)) a -> Evaluator address value (State (EvalContext address) ': State (Exports address) ': effects) a
handleEnv = \case
  Lookup name -> Env.lookupEnv' name . ctxEnvironment <$> get
  Bind name addr -> modify (EvalContext . Env.insertEnv name addr . ctxEnvironment)
  Close names -> Env.intersect names . ctxEnvironment <$> get
  Locally action -> do
    modify' (EvalContext . Env.push @address . ctxEnvironment)
    a <- reinterpret2 handleEnv (raiseEff action)
    a <$ modify' (EvalContext . Env.pop @address . ctxEnvironment)
  GetCtx -> get
  PutCtx e -> put e
  Export name alias addr -> modify (Exports.insert name alias addr)

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
