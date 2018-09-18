{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
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
, freeVariableError
, runEnvironmentError
, runEnvironmentErrorWith
) where

import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.ScopeGraph (Declaration(..))
import Data.Abstract.BaseError
import Data.Abstract.Environment (Bindings, Environment, EvalContext(..), EnvironmentError(..))
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Exports as Exports
import Data.Abstract.Module
import Data.Abstract.Name hiding (name)
import Data.Span
import Prologue

-- | Retrieve the current execution context
getEvalContext :: Member (Env address) effects => Evaluator address value effects (EvalContext address)
getEvalContext = send GetCtx

-- | Retrieve the current environment
getEnv :: Member (Env address) effects
       => Evaluator address value effects (Environment address)
getEnv = ctxEnvironment <$> getEvalContext

-- | Replace the execution context. This is only for use in Analysis.Abstract.Caching.
putEvalContext :: Member (Env address) effects => EvalContext address -> Evaluator address value effects ()
putEvalContext = send . PutCtx

withEvalContext :: Member (Env address) effects
                => EvalContext address
                -> Evaluator address value effects a
                -> Evaluator address value effects a
withEvalContext ctx comp = do
  oldCtx <- getEvalContext
  putEvalContext ctx
  value <- comp
  putEvalContext oldCtx
  pure value

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

self :: Member (Env address) effects => Evaluator address value effects (Maybe address)
self = ctxSelf <$> getEvalContext

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Member (Allocator address) effects
                 , Member (Env address) effects
                 )
              => Name
              -> Evaluator address value effects address
lookupOrAlloc name = lookupEnv name >>= maybeM (alloc name)

letrec :: ( Member (Allocator address) effects
          , Member (Deref value) effects
          , Member (Env address) effects
          , Member (State (Heap address address value)) effects
          , Ord address
          )
       => Declaration
       -> Evaluator address value effects value
       -> Evaluator address value effects (value, address)
letrec declaration body = do
  addr <- lookupOrAlloc (name declaration)
  v <- locally (bind (name declaration) addr *> body)
  assign addr declaration v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( Member (Allocator address) effects
           , Member (Env address) effects
           )
        => Name
        -> (address -> Evaluator address value effects a)
        -> Evaluator address value effects a
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- locally (body addr)
  v <$ bind name addr

-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Member (Env address) effects
            , Member (Reader ModuleInfo) effects
            , Member (Reader Span) effects
            , Member (Resumable (BaseError (EnvironmentError address))) effects
            )
         => Name
         -> Evaluator address value effects address
variable name = lookupEnv name >>= maybeM (freeVariableError name)

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
       => EvalContext address
       -> Evaluator address value (Env address ': effects) a
       -> Evaluator address value effects (Bindings address, a)
runEnv initial = fmap (filterEnv . fmap (first (Env.head . ctxEnvironment))) . runState lowerBound . runState initial . reinterpret2 handleEnv
  where -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
  filterEnv (ports, (binds, a))
          | Exports.null ports = (binds, a)
          | otherwise          = (Exports.toBindings ports <> Env.aliasBindings (Exports.aliases ports) binds, a)

handleEnv :: forall address value effects a . Effects effects
          => Env address (Eff (Env address ': effects)) a
          -> Evaluator address value (State (EvalContext address) ': State (Exports address) ': effects) a
handleEnv = \case
  Lookup name -> Env.lookupEnv' name . ctxEnvironment <$> get
  Bind name addr -> modify (\EvalContext{..} -> EvalContext ctxSelf (Env.insertEnv name addr ctxEnvironment))
  Close names -> Env.intersect names . ctxEnvironment <$> get
  Locally action -> do
    modify' (\EvalContext{..} -> EvalContext ctxSelf (Env.push @address ctxEnvironment))
    a <- reinterpret2 handleEnv (raiseEff action)
    a <$ modify' (\EvalContext{..} -> EvalContext ctxSelf (Env.pop @address ctxEnvironment))
  GetCtx -> get
  PutCtx e -> put e
  Export name alias addr -> modify (Exports.insert name alias addr)

freeVariableError :: ( Member (Reader ModuleInfo) effects
                     , Member (Reader Span) effects
                     , Member (Resumable (BaseError (EnvironmentError address))) effects
                     )
                  => Name
                  -> Evaluator address value effects address
freeVariableError = throwEnvironmentError . FreeVariable

runEnvironmentError :: (Effectful (m address value), Effects effects)
                    => m address value (Resumable (BaseError (EnvironmentError address)) ': effects) a
                    -> m address value effects (Either (SomeExc (BaseError (EnvironmentError address))) a)
runEnvironmentError = runResumable

runEnvironmentErrorWith :: (Effectful (m address value), Effects effects)
                        => (forall resume . BaseError (EnvironmentError address) resume -> m address value effects resume)
                        -> m address value (Resumable (BaseError (EnvironmentError address)) ': effects) a
                        -> m address value effects a
runEnvironmentErrorWith = runResumableWith

throwEnvironmentError :: ( Member (Resumable (BaseError (EnvironmentError address))) effects
                         , Member (Reader ModuleInfo) effects
                         , Member (Reader Span) effects
                         )
                      => EnvironmentError address resume
                      -> Evaluator address value effects resume
throwEnvironmentError = throwBaseError
