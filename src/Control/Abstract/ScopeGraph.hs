{-# LANGUAGE GADTs, KindSignatures, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.ScopeGraph
  ( runScopeEnv
  , ScopeEnv
  , lookup
  , declare
  , reference
  , newScope
  , Declaration(..)
  , Reference(..)
  , EdgeLabel(..)
  , currentScope
  ) where

import           Control.Abstract.Evaluator
import           Control.Abstract.Heap
import           Data.Abstract.Name
import           Data.Abstract.ScopeGraph (Declaration (..), EdgeLabel, Reference, ScopeGraph)
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

data ScopeEnv address (m :: * -> *) a where
    Lookup :: Reference -> ScopeEnv address m (Maybe address)
    Declare :: Declaration -> Span -> ScopeEnv address m ()
    Reference :: Reference -> Declaration -> ScopeEnv address m ()
    Create :: Map EdgeLabel [address] -> m a -> ScopeEnv address m a
    CurrentScope :: ScopeEnv address m (Maybe address)

lookup :: forall address value effects. Member (ScopeEnv address) effects => Reference -> Evaluator address value effects (Maybe address)
lookup = send . Lookup @address

declare :: forall address value effects. Member (ScopeEnv address) effects => Declaration -> Span -> Evaluator address value effects ()
declare = (send .) . Declare @address

reference :: forall address value effects. Member (ScopeEnv address) effects => Reference -> Declaration -> Evaluator address value effects ()
reference = (send .) . Reference @address

newScope :: forall address value effects m a. (Effectful m, Member (ScopeEnv address) effects) => Map EdgeLabel [address] -> m effects a -> Evaluator address value effects a
newScope map action= send (Create map (lowerEff action))

currentScope :: forall address value effects. Member (ScopeEnv address) effects => Evaluator address value effects (Maybe address)
currentScope = send CurrentScope

instance PureEffect (ScopeEnv address)
instance Effect (ScopeEnv address) where
  handleState c dist (Request (Lookup ref) k)         = Request (Lookup ref) (dist . (<$ c) . k)
  handleState c dist (Request (Declare decl ddata) k) = Request (Declare decl ddata) (dist . (<$ c) . k)
  handleState c dist (Request (Reference ref decl) k) = Request (Reference ref decl) (dist . (<$ c) . k)
  handleState c dist (Request (Create edges action) k) = Request (Create edges (dist (action <$ c))) (dist . fmap k)
  handleState c dist (Request CurrentScope k)         = Request CurrentScope (dist . (<$ c) . k)

runScopeEnv :: (Ord address, Effects effects, Member Fresh effects, Member (Allocator address) effects)
            => Evaluator address value (ScopeEnv address ': effects) a
            -> Evaluator address value effects (ScopeGraph address, a)
runScopeEnv evaluator = runState (ScopeGraph.emptyGraph) (reinterpret handleScopeEnv evaluator)

handleScopeEnv :: forall address value effects a. (Ord address, Member Fresh effects, Member (Allocator address) effects, Effects effects)
          => ScopeEnv address (Eff (ScopeEnv address ': effects)) a
          -> Evaluator address value (State (ScopeGraph address) ': effects) a
handleScopeEnv = \case
    Lookup ref -> ScopeGraph.scopeOfRef ref <$> get
    Declare decl ddata -> modify @(ScopeGraph address) (ScopeGraph.declare decl ddata)
    Reference ref decl -> modify @(ScopeGraph address) (ScopeGraph.reference ref decl)
    Create edges action -> do
        -- Take the edges and construct a new scope, update the current scope to the new scope
        currentScope' <- ScopeGraph.currentScope <$> get
        name <- gensym
        address <- alloc name
        modify @(ScopeGraph address) (ScopeGraph.create address edges)
        value <- reinterpret handleScopeEnv (raiseEff action)
        modify @(ScopeGraph address) (\g -> g { ScopeGraph.currentScope = currentScope' })
        pure value
    CurrentScope -> ScopeGraph.currentScope <$> get
