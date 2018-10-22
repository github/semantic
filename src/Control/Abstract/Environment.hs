{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Environment
( Environment
, Exports
, getEvalContext
, putEvalContext
, withEvalContext
, getEnv
, export
, lookupEnv
, bind
, bindAll
, locally
, close
, self
, letrec
, letrec'
, variable
-- * Effects
, Env(..)
, runEnv
, EnvC(..)
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Abstract.BaseError
import Data.Abstract.Environment (Bindings, Environment, EvalContext(..), EnvironmentError(..))
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Exports as Exports
import Data.Abstract.Module
import Data.Abstract.Name
import Data.Span
import Prologue

-- | Retrieve the current execution context
getEvalContext :: (Member (Env address) sig, Carrier sig m) => Evaluator term address value m (EvalContext address)
getEvalContext = send (GetCtx ret)

-- | Retrieve the current environment
getEnv :: (Member (Env address) sig, Carrier sig m)
       => Evaluator term address value m (Environment address)
getEnv = ctxEnvironment <$> getEvalContext

-- | Replace the execution context. This is only for use in Analysis.Abstract.Caching.
putEvalContext :: (Member (Env address) sig, Carrier sig m) => EvalContext address -> Evaluator term address value m ()
putEvalContext context = send (PutCtx context (ret ()))

withEvalContext :: (Member (Env address) sig, Carrier sig m)
                => EvalContext address
                -> Evaluator term address value m a
                -> Evaluator term address value m a
withEvalContext ctx comp = do
  oldCtx <- getEvalContext
  putEvalContext ctx
  value <- comp
  putEvalContext oldCtx
  pure value

-- | Add an export to the global export state.
export :: (Member (Env address) sig, Carrier sig m) => Name -> Name -> Maybe address -> Evaluator term address value m ()
export name alias addr = send (Export name alias addr (ret ()))


-- | Look a 'Name' up in the current environment, trying the default environment if no value is found.
lookupEnv :: (Member (Env address) sig, Carrier sig m) => Name -> Evaluator term address value m (Maybe address)
lookupEnv name = send (Lookup name ret)

-- | Bind a 'Name' to an address in the current scope.
bind :: (Member (Env address) sig, Carrier sig m) => Name -> address -> Evaluator term address value m ()
bind name addr = send (Bind name addr (ret ()))

-- | Bind all of the names from an 'Environment' in the current scope.
bindAll :: (Member (Env address) sig, Carrier sig m) => Bindings address -> Evaluator term address value m ()
bindAll = foldr ((>>) . uncurry bind) (pure ()) . Env.pairs

-- | Run an action in a new local scope.
locally :: forall term address value sig m a . (Member (Env address) sig, Carrier sig m) => Evaluator term address value m a -> Evaluator term address value m a
locally m = send (Locally @address m ret)

close :: (Member (Env address) sig, Carrier sig m) => Set Name -> Evaluator term address value m (Environment address)
close fvs = send (Close fvs ret)

self :: (Member (Env address) sig, Carrier sig m) => Evaluator term address value m (Maybe address)
self = ctxSelf <$> getEvalContext

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Member (Allocator address) sig
                 , Member (Env address) sig
                 , Carrier sig m
                 )
              => Name
              -> Evaluator term address value m address
lookupOrAlloc name = lookupEnv name >>= maybeM (alloc name)

letrec :: ( Member (Allocator address) sig
          , Member (Deref value) sig
          , Member (Env address) sig
          , Member (State (Heap address value)) sig
          , Ord address
          , Carrier sig m
          )
       => Name
       -> Evaluator term address value m value
       -> Evaluator term address value m (value, address)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- locally (bind name addr *> body)
  assign addr v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( Member (Allocator address) sig
           , Member (Env address) sig
           , Carrier sig m
           )
        => Name
        -> (address -> Evaluator term address value m a)
        -> Evaluator term address value m a
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- locally (body addr)
  v <$ bind name addr

-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Member (Env address) sig
            , Member (Reader ModuleInfo) sig
            , Member (Reader Span) sig
            , Member (Resumable (BaseError (EnvironmentError address))) sig
            , Carrier sig m
            )
         => Name
         -> Evaluator term address value m address
variable name = lookupEnv name >>= maybeM (freeVariableError name)

-- Effects

data Env address m k
  = Lookup Name (Maybe address -> k)
  | Bind Name address k
  | Close (Set Name) (Environment address -> k)
  | forall a . Locally (m a) (a -> k)
  | GetCtx (EvalContext address -> k)
  | PutCtx (EvalContext address) k
  | Export Name Name (Maybe address) k

deriving instance Functor (Env address m)

instance HFunctor (Env address) where
  hmap _ (Lookup name k) = Lookup name k
  hmap _ (Bind name addr k) = Bind name addr k
  hmap _ (Close names k) = Close names k
  hmap f (Locally m k) = Locally (f m) k
  hmap _ (GetCtx k) = GetCtx k
  hmap _ (PutCtx ctx k) = PutCtx ctx k
  hmap _ (Export name alias addr k) = Export name alias addr k

instance Effect (Env address) where
  handle state handler (Lookup name k) = Lookup name (handler . (<$ state) . k)
  handle state handler (Bind name addr k) = Bind name addr (handler . (<$ state) $ k)
  handle state handler (Close names k) = Close names (handler . (<$ state) . k)
  handle state handler (Locally action k) = Locally (handler (action <$ state)) (handler . fmap k)
  handle state handler (GetCtx k) = GetCtx (handler . (<$ state) . k)
  handle state handler (PutCtx e k) = PutCtx e (handler . (<$ state) $ k)
  handle state handler (Export name alias addr k) = Export name alias addr (handler . (<$ state) $ k)

-- | Runs a computation in the context of an existing environment.
--   New bindings created in the computation are returned.
runEnv :: (Carrier sig m, Effect sig)
       => EvalContext address
       -> Evaluator term address value (EnvC (Eff
                                       (StateC (EvalContext address) (Eff
                                       (StateC (Exports address) (Eff
                                       m)))))) a
       -> Evaluator term address value m (Bindings address, a)
runEnv initial = Evaluator . fmap (filterEnv . fmap (first (Env.head . ctxEnvironment))) . runState lowerBound . runState initial . runEnvC . interpret . runEvaluator
  where -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
  filterEnv (ports, (binds, a))
          | Exports.null ports = (binds, a)
          | otherwise          = (Exports.toBindings ports <> Env.aliasBindings (Exports.aliases ports) binds, a)

newtype EnvC m a = EnvC { runEnvC :: m a }

instance (Carrier (State (EvalContext address) :+: State (Exports address) :+: sig) m, HFunctor sig, Monad m) => Carrier (Env address :+: sig) (EnvC m) where
  ret = EnvC . ret
  eff = EnvC . (alg \/ (eff . R . R . handlePure runEnvC))
    where alg = \case
            Lookup name k -> gets (Env.lookupEnv' name . ctxEnvironment) >>= runEnvC . k
            Bind name addr k -> modify (\EvalContext{..} -> EvalContext ctxSelf (Env.insertEnv name addr ctxEnvironment)) >> runEnvC k
            Close names k -> gets (Env.intersect names . ctxEnvironment) >>= runEnvC . k
            Locally action k -> do
              modify (\EvalContext{..} -> EvalContext ctxSelf (Env.push @address ctxEnvironment))
              a <- runEnvC action
              modify (\EvalContext{..} -> EvalContext ctxSelf (Env.pop @address ctxEnvironment))
              runEnvC (k a)
            GetCtx k -> get >>= runEnvC . k
            PutCtx e k -> put e >> runEnvC k
            Export name alias addr k -> modify (Exports.insert name alias addr) >> runEnvC k

freeVariableError :: ( Member (Reader ModuleInfo) sig
                     , Member (Reader Span) sig
                     , Member (Resumable (BaseError (EnvironmentError address))) sig
                     , Carrier sig m
                     )
                  => Name
                  -> Evaluator term address value m address
freeVariableError = throwEnvironmentError . FreeVariable

runEnvironmentError :: (Carrier sig m, Effect sig)
                    => Evaluator term address value (ResumableC (BaseError (EnvironmentError address)) (Evaluator term address value m)) a
                    -> Evaluator term address value m (Either (SomeError (BaseError (EnvironmentError address))) a)
runEnvironmentError = runResumable . runEvaluator

runEnvironmentErrorWith :: Carrier sig m
                        => (forall resume . BaseError (EnvironmentError address) resume -> Evaluator term address value m resume)
                        -> Evaluator term address value (ResumableWithC (BaseError (EnvironmentError address)) (Evaluator term address value m)) a
                        -> Evaluator term address value m a
runEnvironmentErrorWith f = runResumableWith f . runEvaluator

throwEnvironmentError :: ( Member (Resumable (BaseError (EnvironmentError address))) sig
                         , Member (Reader ModuleInfo) sig
                         , Member (Reader Span) sig
                         , Carrier sig m
                         )
                      => EnvironmentError address resume
                      -> Evaluator term address value m resume
throwEnvironmentError = throwBaseError
