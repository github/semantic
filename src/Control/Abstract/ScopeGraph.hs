{-# LANGUAGE GADTs, KindSignatures, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.ScopeGraph
  ( lookup
  , declare
  , reference
  , newScope
  , Declaration(..)
  , ScopeGraph
  , ScopeError
  , name
  , Reference(..)
  , EdgeLabel(..)
  , currentScope
  , withScope
  , associatedScope
  , putDeclarationScope
  ) where

import           Control.Abstract.Evaluator hiding (Local)
import           Data.Abstract.Module
import           Data.Abstract.BaseError
import           Data.Abstract.Name hiding (name)
import           Data.Abstract.ScopeGraph (Declaration (..), EdgeLabel, Reference, ScopeGraph, Address(..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

lookup :: (Ord address, Member (State (ScopeGraph address)) effects) => Reference -> Evaluator address value effects (Maybe address)
lookup ref = ScopeGraph.scopeOfRef ref <$> get

declare :: ( Ord address
           , Member (Resumable (BaseError (ScopeError address))) effects
           , Member (Reader ModuleInfo) effects
           , Member (Reader Span) effects
           , Member (State (ScopeGraph address)) effects
           )
        => Declaration
        -> Span
        -> Maybe address
        -> Evaluator address value effects (Address address)
declare decl span scope = do
  graph <- get
  let (graph', position) = ScopeGraph.declare decl span scope graph
  put graph'
  currentScope <- ScopeGraph.currentScope <$> get
  case (currentScope, position) of
    (Just scope, Just position) -> pure (Address scope position)
    _ -> throwScopeError (ScopeError decl span)

putDeclarationScope :: (Ord address, Member (State (ScopeGraph address)) effects) => Declaration -> address -> Evaluator address value effects ()
putDeclarationScope decl = modify . (ScopeGraph.insertDeclarationScope decl)

reference :: forall address effects value. (Ord address, Member (State (ScopeGraph address)) effects) => Reference -> Declaration -> Evaluator address value effects ()
reference ref = modify @(ScopeGraph address) . (ScopeGraph.reference ref)

newScope :: (Member Fresh effects, Member (Allocator address) effects, Ord address, Member (State (ScopeGraph address)) effects) => Map EdgeLabel [address]  -> Evaluator address value effects address
newScope edges = do
  -- Take the edges and construct a new scope, update the current scope to the new scope
  name <- gensym
  address <- alloc name
  address <$ modify (ScopeGraph.newScope address edges)

currentScope :: Member (State (ScopeGraph address)) effects => Evaluator address value effects (Maybe address)
currentScope = ScopeGraph.currentScope <$> get

associatedScope :: (Ord address, Member (State (ScopeGraph address)) effects) => Declaration -> Evaluator address value effects (Maybe address)
associatedScope decl = ScopeGraph.associatedScope decl <$> get

withScope :: forall m address value effects a. (Effectful (m address value), Member (State (ScopeGraph address)) effects) => address -> m address value effects a -> m address value effects a
withScope scope action = raiseEff $ do
    prevScope <- (lowerEff (currentScope @address))
    modify (\g -> g { ScopeGraph.currentScope = Just scope })
    value <- (lowerEff action)
    modify (\g -> g { ScopeGraph.currentScope = prevScope })
    pure value

throwScopeError :: ( Member (Resumable (BaseError (ScopeError address))) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   )
            => ScopeError address resume
            -> Evaluator address value effects resume
throwScopeError = throwBaseError

data ScopeError address return where
  ScopeError :: Declaration -> Span -> ScopeError address (Address address)

deriving instance Eq (ScopeError address return)
deriving instance Show (ScopeError address return)
instance Show address => Show1 (ScopeError address) where liftShowsPrec _ _ = showsPrec
instance Eq address => Eq1 (ScopeError address) where liftEq _ (ScopeError m1 n1) (ScopeError m2 n2) = m1 == m2 && n1 == n2
