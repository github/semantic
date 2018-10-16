{-# LANGUAGE ExistentialQuantification, GADTs, KindSignatures, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Abstract.ScopeGraph
  ( runScopeEnv
  , ScopeEnvC(..)
  , ScopeEnv
  , lookup
  , declare
  , reference
  , newScope
  , Declaration(..)
  , Reference(..)
  , EdgeLabel(..)
  , currentScope
  , withScope
  , associatedScope
  , putDeclarationScope
  ) where

import           Control.Abstract.Evaluator hiding (Local)
import           Control.Abstract.Heap
import           Data.Abstract.Name
import           Data.Abstract.ScopeGraph (Declaration (..), EdgeLabel, Reference, ScopeGraph)
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

data ScopeEnv address (m :: * -> *) k
  = Lookup Reference (Maybe address -> k)
  | Declare Declaration Span (Maybe address) k
  | PutDeclarationScope Declaration address k
  | Reference Reference Declaration k
  | NewScope (Map EdgeLabel [address]) (address -> k)
  | CurrentScope (Maybe address -> k)
  | forall a . Local address (m a) (a -> k)
  | AssociatedScope Declaration (Maybe address -> k)

deriving instance Functor (ScopeEnv address m)

lookup :: (Member (ScopeEnv address) sig, Carrier sig m) => Reference -> Evaluator term address value m (Maybe address)
lookup ref = sendScope (Lookup ref gen)

declare :: (Member (ScopeEnv address) sig, Carrier sig m) => Declaration -> Span -> Maybe address -> Evaluator term address value m ()
declare decl span addr = sendScope (Declare decl span addr (gen ()))

putDeclarationScope :: (Member (ScopeEnv address) sig, Carrier sig m) => Declaration -> address -> Evaluator term address value m ()
putDeclarationScope decl addr = sendScope (PutDeclarationScope decl addr (gen ()))

reference :: (Member (ScopeEnv address) sig, Carrier sig m) => Reference -> Declaration -> Evaluator term address value m ()
reference ref decl = sendScope (Reference ref decl (gen ()))

newScope :: (Member (ScopeEnv address) sig, Carrier sig m) => Map EdgeLabel [address]  -> Evaluator term address value m address
newScope map = send (NewScope map gen)

currentScope :: (Member (ScopeEnv address) sig, Carrier sig m) => Evaluator term address value m (Maybe address)
currentScope = send (CurrentScope gen)

associatedScope :: (Member (ScopeEnv address) sig, Carrier sig m) => Declaration -> Evaluator term address value m (Maybe address)
associatedScope = send . flip AssociatedScope gen

withScope :: (Member (ScopeEnv address) sig, Carrier sig m) => address -> Evaluator term address value m a -> Evaluator term address value m a
withScope scope action = send (Local scope action gen)

sendScope :: (Member (ScopeEnv address) sig, Carrier sig m) => ScopeEnv address (Evaluator term address value m) (Evaluator term address value m a) -> Evaluator term address value m a
sendScope = send

instance HFunctor (ScopeEnv address) where
  hmap _ (Lookup ref                          k) = Lookup ref k
  hmap _ (Declare decl span assocScope        k) = Declare decl span assocScope k
  hmap _ (PutDeclarationScope decl assocScope k) = PutDeclarationScope decl assocScope k
  hmap _ (Reference ref decl                  k) = Reference ref decl k
  hmap _ (NewScope edges                      k) = NewScope edges k
  hmap _ (CurrentScope                        k) = CurrentScope k
  hmap _ (AssociatedScope decl                k) = AssociatedScope decl k
  hmap f (Local scope action                  k) = Local scope (f action) k

instance Effect (ScopeEnv address) where
  handle state handler (Lookup ref                          k) = Lookup ref (handler . (<$ state) . k)
  handle state handler (Declare decl span assocScope        k) = Declare decl span assocScope (handler (k <$ state))
  handle state handler (PutDeclarationScope decl assocScope k) = PutDeclarationScope decl assocScope (handler (k <$ state))
  handle state handler (Reference ref decl                  k) = Reference ref decl (handler (k <$ state))
  handle state handler (NewScope edges                      k) = NewScope edges (handler . (<$ state) . k)
  handle state handler (CurrentScope                        k) = CurrentScope (handler . (<$ state) . k)
  handle state handler (AssociatedScope decl                k) = AssociatedScope decl (handler . (<$ state) . k)
  handle state handler (Local scope action                  k) = Local scope (handler (action <$ state)) (handler . fmap k)


runScopeEnv :: (Ord address, Member Fresh sig, Member (Allocator address) sig, Carrier sig m, Effect sig)
            => Evaluator term address value (ScopeEnvC
              (Evaluator term address value (StateC (ScopeGraph address)
              (Evaluator term address value m)))) a
            -> Evaluator term address value m (ScopeGraph address, a)
runScopeEnv = runState lowerBound . runEvaluator . runScopeEnvC . interpret . runEvaluator

newtype ScopeEnvC m a = ScopeEnvC { runScopeEnvC :: m a }

instance (Ord address, Member Fresh sig, Member (Allocator address) sig, Carrier (State (ScopeGraph address) :+: sig) m, Effect sig) => Carrier (ScopeEnv address :+: sig) (ScopeEnvC (Evaluator term address value m)) where
  gen = ScopeEnvC . gen
  alg = ScopeEnvC . (algS \/ (alg . R . handlePure runScopeEnvC))
    where algS (Lookup ref k) = gets (ScopeGraph.scopeOfRef ref) >>= runScopeEnvC . k
          algS (Declare decl span scope k) = modify' @(ScopeGraph address) (ScopeGraph.declare decl span scope) *> runScopeEnvC k
          algS (PutDeclarationScope decl scope k) = modify' @(ScopeGraph address) (ScopeGraph.insertDeclarationScope decl scope) *> runScopeEnvC k
          algS (Reference ref decl k) = modify' @(ScopeGraph address) (ScopeGraph.reference ref decl) *> runScopeEnvC k
          algS (NewScope edges k) = do
            -- Take the edges and construct a new scope, update the current scope to the new scope
            name <- gensym
            address <- alloc name
            modify' @(ScopeGraph address) (ScopeGraph.newScope address edges)
            runScopeEnvC (k address)
          algS (CurrentScope k) = gets ScopeGraph.currentScope >>= runScopeEnvC . k
          algS (AssociatedScope decl k) = gets (ScopeGraph.associatedScope decl) >>= runScopeEnvC . k
          algS (Local scope action k) = do
            prevScope <- gets ScopeGraph.currentScope
            modify' @(ScopeGraph address) (\g -> g { ScopeGraph.currentScope = Just scope })
            value <- runScopeEnvC action
            modify @(ScopeGraph address) (\g -> g { ScopeGraph.currentScope = prevScope })
            runScopeEnvC (k value)
