{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Abstract.ScopeGraph
  ( lookup
  , declare
  , reference
  , newScope
  , bindAll
  , Declaration(..)
  , ScopeGraph
  , ScopeError(..)
  , Reference(..)
  , EdgeLabel(..)
  , currentScope
  , insertExportEdge
  , insertImportEdge
  , insertLexicalEdge
  , withScope
  , associatedScope
  , putDeclarationScope
  , putCurrentScope
  , insertImportReference
  , lookupScopePath
  , lookupDeclarationScope
  , lookupScope
  , Allocator(..)
  , AllocatorC(..)
  , runAllocator
  , alloc
  , Address(..)
  , runScopeErrorWith
  , runScopeError
  , throwScopeError
  , Scope
  , ScopeGraph.Path
  ) where

import           Control.Abstract.Evaluator hiding (Local)
import           Data.Abstract.Module
import           Data.Abstract.BaseError
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Data.Abstract.Name hiding (name)
import           Data.Abstract.ScopeGraph (Declaration (..), EdgeLabel, Reference, ScopeGraph, Address(..), Scope(..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Map.Strict as Map
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

lookup :: (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Reference -> Evaluator term address value m (Maybe address)
lookup ref = ScopeGraph.scopeOfRef ref <$> get

-- TODO: Don't return an address.
declare :: ( Carrier sig m
           , Member (State (ScopeGraph address)) sig
           , Ord address
           )
        => Declaration
        -> Span
        -> Maybe address
        -> Evaluator term address value m ()
declare decl span scope = modify (fst . ScopeGraph.declare decl span scope)

putDeclarationScope :: (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Declaration -> address -> Evaluator term address value m ()
putDeclarationScope decl = modify . (ScopeGraph.insertDeclarationScope decl)

reference :: forall address sig m term value. (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Reference -> Declaration -> Evaluator term address value m ()
reference ref = modify @(ScopeGraph address) . (ScopeGraph.reference ref)

-- | Combinator to insert an export edge from the current scope to the provided scope address.
insertExportEdge :: (Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
                 => scopeAddress
                 -> Evaluator term scopeAddress value m ()
insertExportEdge = insertEdge ScopeGraph.Export

-- | Combinator to insert an import edge from the current scope to the provided scope address.
insertImportEdge :: (Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
                 => scopeAddress
                 -> Evaluator term scopeAddress value m ()
insertImportEdge = insertEdge ScopeGraph.Import

-- | Combinator to insert a lexical edge from the current scope to the provided scope address.
insertLexicalEdge :: (Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
                  => scopeAddress
                  -> Evaluator term scopeAddress value m ()
insertLexicalEdge = insertEdge ScopeGraph.Lexical

insertEdge :: (Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
           => EdgeLabel
           -> scopeAddress
           -> Evaluator term scopeAddress value m ()
insertEdge label target = modify (ScopeGraph.insertEdge label target)

-- | Bind all of the scopes from a 'ScopeGraph'.
bindAll :: ( Ord address
           , Member (Reader ModuleInfo) sig
           , Member (Reader Span) sig
           , Member (Resumable (BaseError (ScopeError address))) sig
           , Member (State (ScopeGraph address)) sig
           , Carrier sig m
           )
        => ScopeGraph address
        -> Evaluator term address value m ()
bindAll oldGraph = do
  currentGraph <- get
  let newGraph = ScopeGraph.graph oldGraph <> ScopeGraph.graph currentGraph
  put (currentGraph { ScopeGraph.graph = newGraph })

-- | Inserts a new scope into the scope graph with the given edges.
newScope :: ( Member (Allocator address) sig
            , Member (State (ScopeGraph address)) sig
            , Member Fresh sig
            , Carrier sig m
            , Ord address
            )
         => Map EdgeLabel [address]
         -> Evaluator term address value m address
newScope edges = do
  -- Take the edges and construct a new scope
  name <- gensym
  address <- alloc name
  address <$ modify (ScopeGraph.newScope address edges)

currentScope :: ( Member (State (ScopeGraph address)) sig
                , Carrier sig m
                )
             => Evaluator term address value m (Maybe address)
currentScope = ScopeGraph.currentScope <$> get

lookupScope :: ( Member (Resumable (BaseError (ScopeError address))) sig
                , Member (Reader ModuleInfo) sig
                , Member (Reader Span) sig
                , Member (State (ScopeGraph address)) sig
                , Carrier sig m
                , Ord address
                )
             => address
             -> Evaluator term address value m (Scope address)
lookupScope address = maybeM (throwScopeError LookupScopeError) . ScopeGraph.lookupScope address =<< get

insertImportReference :: ( Member (Resumable (BaseError (ScopeError address))) sig
                        , Member (Reader ModuleInfo) sig
                        , Member (Reader Span) sig
                        , Member (State (ScopeGraph address)) sig
                        , Carrier sig m
                        , Ord address
                        )
                      => Reference
                      -> Declaration
                      -> ScopeGraph address
                      -> address
                      -> Scope address
                      -> Evaluator term address value m ()
insertImportReference ref decl g scopeAddress scope = do
  newScope <- maybeM (throwScopeError LookupScopeError) (ScopeGraph.insertImportReference ref decl g scopeAddress scope)
  insertScope scopeAddress newScope

insertScope :: ( Member (Resumable (BaseError (ScopeError address))) sig
               , Member (Reader ModuleInfo) sig
               , Member (Reader Span) sig
               , Member (State (ScopeGraph address)) sig
               , Carrier sig m
               , Ord address
               )
            => address
            -> Scope address
            -> Evaluator term address value m ()
insertScope scopeAddress scope = modify (ScopeGraph.insertScope scopeAddress scope)

lookupScopePath :: ( Member (Resumable (BaseError (ScopeError address))) sig
                , Member (Reader ModuleInfo) sig
                , Member (Reader Span) sig
                , Member (State (ScopeGraph address)) sig
                , Carrier sig m
                , Ord address
                , Show address
                )
             => Declaration
             -> Evaluator term address value m (ScopeGraph.Path address)
lookupScopePath decl@Declaration{..} = maybeM (throwScopeError $ LookupPathError decl) . ScopeGraph.lookupScopePath unDeclaration =<< get

lookupDeclarationScope :: ( Member (Resumable (BaseError (ScopeError address))) sig
                , Member (Reader ModuleInfo) sig
                , Member (Reader Span) sig
                , Member (State (ScopeGraph address)) sig
                , Carrier sig m
                , Ord address
                , Show address
                ) => Declaration -> Evaluator term address value m address
lookupDeclarationScope decl = do
  path <- lookupScopePath decl
  currentScope' <- currentScope
  maybeM (throwScopeError $ LookupDeclarationScopeError decl) (ScopeGraph.pathDeclarationScope currentScope' path)

associatedScope :: (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Declaration -> Evaluator term address value m (Maybe address)
associatedScope decl = ScopeGraph.associatedScope decl <$> get

withScope :: ( Carrier sig m
             , Member (State (ScopeGraph address)) sig
             )
          => address
          -> Evaluator term address value m a
          -> Evaluator term address value m a
withScope scope action = do
    prevScope <- currentScope
    modify (\g -> g { ScopeGraph.currentScope = Just scope })
    value <- action
    case prevScope of
      Nothing -> modify (\g -> g { ScopeGraph.currentScope = Just scope })
      _ -> modify (\g -> g { ScopeGraph.currentScope = prevScope })
    pure value

putCurrentScope :: (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => address -> Evaluator term address value m ()
putCurrentScope scope = modify (\g -> g { ScopeGraph.currentScope = Just scope })

throwScopeError :: ( Member (Resumable (BaseError (ScopeError address))) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Carrier sig m
                   )
            => ScopeError address resume
            -> Evaluator term address value m resume
throwScopeError = throwBaseError

data ScopeError address return where
  ScopeError :: Declaration -> Span -> ScopeError address (Address address)
  LookupScopeError :: ScopeError address (Scope address)
  LookupPathError :: Declaration -> ScopeError address (ScopeGraph.Path address)
  LookupDeclarationScopeError :: Declaration -> ScopeError address address
  CurrentScopeError :: ScopeError address address

deriving instance Eq (ScopeError address return)
deriving instance Show (ScopeError address return)
instance Show address => Show1 (ScopeError address) where liftShowsPrec _ _ = showsPrec
instance Eq address => Eq1 (ScopeError address) where
  liftEq _ (ScopeError m1 n1) (ScopeError m2 n2) = m1 == m2 && n1 == n2
  liftEq _ CurrentScopeError CurrentScopeError = True
  liftEq _ LookupScopeError LookupScopeError = True
  liftEq _ (LookupPathError decl1) (LookupPathError decl2) = decl1 == decl2
  liftEq _ _ _ = False

alloc :: (Member (Allocator address) sig, Carrier sig m) => Name -> Evaluator term address value m address
alloc = send . flip Alloc ret

data Allocator address (m :: * -> *) k
  = Alloc Name (address -> k)
  deriving (Functor)

instance HFunctor (Allocator address) where
  hmap _ (Alloc name k) = Alloc name k

instance Effect (Allocator address) where
  handle state handler (Alloc name k) = Alloc name (handler . (<$ state) . k)

runAllocator :: Carrier (Allocator address :+: sig) (AllocatorC address (Eff m))
             => Evaluator term address value (AllocatorC address (Eff m)) a
             -> Evaluator term address value m a
runAllocator = raiseHandler $ runAllocatorC . interpret

newtype AllocatorC address m a = AllocatorC { runAllocatorC :: m a }
  deriving (Alternative, Applicative, Functor, Monad)


runScopeErrorWith :: Carrier sig m
                  => (forall resume . BaseError (ScopeError address) resume -> Evaluator term address value m resume)
                  -> Evaluator term address value (ResumableWithC (BaseError (ScopeError address)) (Eff m)) a
                  -> Evaluator term address value m a
runScopeErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

runScopeError :: (Carrier sig m, Effect sig)
              => Evaluator term address value (ResumableC (BaseError (ScopeError address)) (Eff m)) a
              -> Evaluator term address value m (Either (SomeError (BaseError (ScopeError address))) a)
runScopeError = raiseHandler runResumable
