{-# LANGUAGE LambdaCase, TypeOperators, GADTs, KindSignatures, ScopedTypeVariables, RankNTypes #-}
module Control.Abstract.ScopeGraph where

import Data.Abstract.ScopeGraph as ScopeGraph
import Control.Monad.Effect
import Control.Abstract.Evaluator

data ScopeEnv address ddata (m :: * -> *) a where
    Lookup :: Reference -> ScopeEnv address ddata m (Maybe address)
    Declare :: Declaration -> ddata -> ScopeEnv address ddata m ()
    Reference :: Reference -> Declaration -> ScopeEnv address ddata m ()

instance PureEffect (ScopeEnv address ddata)
instance Effect (ScopeEnv address ddata) where
  handleState c dist (Request (Lookup ref) k) = Request (Lookup ref) (dist . (<$ c) . k)
  handleState c dist (Request (Declare decl ddata) k) = Request (Declare decl ddata) (dist . (<$ c) . k)
  handleState c dist (Request (Reference ref decl) k) = Request (Reference ref decl) (dist . (<$ c) . k)

runScopeEnv :: (Ord scope, Effects effects)
            => scope
            -> Evaluator address value (ScopeEnv scope ddata ': effects) a
            -> Evaluator address value effects (ScopeGraph scope ddata, a)
runScopeEnv scope = runState (ScopeGraph.emptyGraph scope) . reinterpret handleScopeEnv

handleScopeEnv :: forall scope address ddata value effects a. (Ord scope)
          => ScopeEnv scope ddata (Eff (ScopeEnv scope ddata ': effects)) a
          -> Evaluator address value (State (ScopeGraph scope ddata) ': effects) a
handleScopeEnv = \case
    Lookup ref -> do
        graph <- get @(ScopeGraph scope ddata)
        pure (ScopeGraph.scopeOfRef ref graph)
    Declare decl ddata -> do
        graph <- get
        put @(ScopeGraph scope ddata) (ScopeGraph.declare decl ddata graph)
        pure ()
    Reference ref decl -> do
        graph <- get
        put @(ScopeGraph scope ddata) (ScopeGraph.reference reference decl graph)
        pure ()
