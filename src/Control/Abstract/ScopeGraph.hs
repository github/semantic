{-# LANGUAGE LambdaCase, TypeOperators, GADTs, KindSignatures, ScopedTypeVariables, RankNTypes #-}
module Control.Abstract.ScopeGraph where

import Data.Abstract.ScopeGraph as ScopeGraph
import Data.Semilattice.Lower
import Control.Monad.Effect
import Control.Abstract.Evaluator

data ScopeEnv address (m :: * -> *) a where
    Lookup :: Reference term -> ScopeEnv address m (Maybe scope)
    Declare :: Declaration term -> ScopeEnv address m ()
    Reference :: Reference term -> Declaration term -> ScopeEnv address m ()

instance PureEffect (ScopeEnv address)
instance Effect (ScopeEnv address) where
  handleState c dist (Request (Lookup ref) k) = Request (Lookup ref) (dist . (<$ c) . k)
  handleState c dist (Request (Declare decl) k) = Request (Declare decl) (dist . (<$ c) . k)
  handleState c dist (Request (Reference ref decl) k) = Request (Reference ref decl) (dist . (<$ c) . k)

runScopeEnv :: (Ord scope, Ord term, Effects effects)
            => Evaluator address value (ScopeEnv scope ': effects) a
            -> Evaluator address value effects (ScopeGraph scope term ddata, a)
runScopeEnv = runState (ScopeGraph mempty) . reinterpret handleScopeEnv

handleScopeEnv :: forall scope address term ddata value effects a. (Ord term, Effects effects)
          => ScopeEnv scope (Eff (ScopeEnv scope ': effects)) a
          -> Evaluator address value (State (ScopeGraph scope term ddata) ': effects) a
handleScopeEnv = \case
    Lookup ref -> do
        graph <- get @(ScopeGraph scope term ddata)
        pure (ScopeGraph.scopeOfRef ref graph)
