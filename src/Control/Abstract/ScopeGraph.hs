{-# LANGUAGE GADTs, KindSignatures, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.ScopeGraph (runScopeEnv, ScopeEnv) where

import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Data.Abstract.Name
import Data.Span
import Data.Abstract.ScopeGraph as ScopeGraph
import Prologue

data ScopeEnv address (m :: * -> *) a where
    Lookup :: Reference -> ScopeEnv address m (Maybe address)
    Declare :: Declaration -> Span -> ScopeEnv address m ()
    Reference :: Reference -> Declaration -> ScopeEnv address m ()
    Create :: Map EdgeLabel [Name] -> ScopeEnv Name m ()

instance PureEffect (ScopeEnv address)
instance Effect (ScopeEnv address) where
  handleState c dist (Request (Lookup ref) k)         = Request (Lookup ref) (dist . (<$ c) . k)
  handleState c dist (Request (Declare decl ddata) k) = Request (Declare decl ddata) (dist . (<$ c) . k)
  handleState c dist (Request (Reference ref decl) k) = Request (Reference ref decl) (dist . (<$ c) . k)
  handleState c dist (Request (Create edges) k) = Request (Create edges) (dist . (<$ c) . k)

runScopeEnv :: (Ord address, Effects effects, Member Fresh effects, Member (Allocator address) effects)
            => Evaluator address value (ScopeEnv address ': effects) a
            -> Evaluator address value effects (ScopeGraph address, a)
runScopeEnv evaluator = do
    name <- gensym
    address <- alloc name
    runState (ScopeGraph.emptyGraph address) (reinterpret handleScopeEnv evaluator)

handleScopeEnv :: forall address value effects a. (Ord address, Member Fresh effects)
          => ScopeEnv address (Eff (ScopeEnv address ': effects)) a
          -> Evaluator address value (State (ScopeGraph address) ': effects) a
handleScopeEnv = \case
    Lookup ref -> do
        graph <- get @(ScopeGraph address)
        pure (ScopeGraph.scopeOfRef ref graph)
    Declare decl ddata -> do
        graph <- get
        put @(ScopeGraph address) (ScopeGraph.declare decl ddata graph)
        pure ()
    Reference ref decl -> do
        graph <- get
        put @(ScopeGraph address) (ScopeGraph.reference ref decl graph)
        pure ()
    Create edges -> do
        graph <- get @(ScopeGraph address)
        scope <- gensym
        put  (ScopeGraph.create scope edges graph)
        pure ()
