{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators,
             UndecidableInstances #-}
module Control.Abstract.ScopeGraph
  ( lookup
  , declare
  , reference
  , newScope
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
  , putDeclarationSpan
  , insertImportReference
  , lookupScopePath
  , maybeLookupScopePath
  , lookupDeclarationScope
  , lookupScope
  , Allocator(..)
  , AllocatorC(..)
  , runAllocator
  , alloc
  , Slot(..)
  , runScopeErrorWith
  , runScopeError
  , throwScopeError
  , Scope
  , ScopeGraph.Path
  ) where

import           Control.Abstract.Evaluator hiding (Local)
import           Control.Effect.Carrier
import           Data.Abstract.BaseError
import           Data.Abstract.Module
import           Data.Abstract.Name hiding (name)
import           Data.Abstract.ScopeGraph (Declaration (..), EdgeLabel, Reference, Scope (..), ScopeGraph, Slot (..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

lookup :: (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Reference -> Evaluator term address value m (Maybe address)
lookup ref = ScopeGraph.scopeOfRef ref <$> get

-- TODO: Don't return an address.
declare :: ( Carrier sig m
           , Member (State (ScopeGraph address)) sig
           , Member (Reader (address, address)) sig
           , Ord address
           )
        => Declaration
        -> Span
        -> Maybe address
        -> Evaluator term address value m ()
declare decl span scope = do
  currentAddress <- currentScope
  modify (fst . ScopeGraph.declare decl span scope currentAddress)

putDeclarationScope :: (Ord address, Member (Reader (address, address)) sig, Member (State (ScopeGraph address)) sig, Carrier sig m) => Declaration -> address -> Evaluator term address value m ()
putDeclarationScope decl assocScope = do
  currentAddress <- currentScope
  modify (ScopeGraph.insertDeclarationScope decl assocScope currentAddress)

putDeclarationSpan :: forall address sig m term value. (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Declaration -> Span -> Evaluator term address value m ()
putDeclarationSpan decl = modify @(ScopeGraph address) . (ScopeGraph.insertDeclarationSpan decl)

reference :: forall address sig m term value
          . ( Ord address
            , Member (State (ScopeGraph address)) sig
            , Member (Reader (address, address)) sig
            , Carrier sig m)
          => Reference
          -> Declaration
          -> Evaluator term address value m ()
reference ref decl = do
  currentAddress <- currentScope
  modify @(ScopeGraph address) (ScopeGraph.reference ref decl currentAddress)

-- | Combinator to insert an export edge from the current scope to the provided scope address.
insertExportEdge :: (Member (Reader (scopeAddress, scopeAddress)) sig, Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
                 => scopeAddress
                 -> Evaluator term scopeAddress value m ()
insertExportEdge = insertEdge ScopeGraph.Export

-- | Combinator to insert an import edge from the current scope to the provided scope address.
insertImportEdge :: (Member (Reader (scopeAddress, scopeAddress)) sig, Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
                 => scopeAddress
                 -> Evaluator term scopeAddress value m ()
insertImportEdge = insertEdge ScopeGraph.Import

-- | Combinator to insert a lexical edge from the current scope to the provided scope address.
insertLexicalEdge :: (Member (Reader (scopeAddress, scopeAddress)) sig, Member (State (ScopeGraph scopeAddress)) sig, Carrier sig m, Ord scopeAddress)
                  => scopeAddress
                  -> Evaluator term scopeAddress value m ()
insertLexicalEdge = insertEdge ScopeGraph.Lexical

insertEdge :: ( Member (State (ScopeGraph address)) sig
              , Member (Reader (address, address)) sig
              , Carrier sig m
              , Ord address)
           => EdgeLabel
           -> address
           -> Evaluator term address value m ()
insertEdge label target = do
  currentAddress <- currentScope
  modify (ScopeGraph.insertEdge label target currentAddress)

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

currentScope :: forall address sig term value m. ( Member (Reader (address, address)) sig
                , Carrier sig m
                )
             => Evaluator term address value m address
currentScope = asks @(address, address) fst

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
                        , Member (Reader (address, address)) sig
                        , Carrier sig m
                        , Ord address
                        )
                      => Reference
                      -> Declaration
                      -> address
                      -> Evaluator term address value m ()
insertImportReference ref decl scopeAddress = do
  scopeGraph <- get
  scope <- lookupScope scopeAddress
  currentAddress <- currentScope
  newScope <- maybeM (throwScopeError LookupScopeError) (ScopeGraph.insertImportReference ref decl currentAddress scopeGraph scope)
  insertScope scopeAddress newScope

insertScope :: ( Member (State (ScopeGraph address)) sig
               , Carrier sig m
               , Ord address
               )
            => address
            -> Scope address
            -> Evaluator term address value m ()
insertScope scopeAddress scope = modify (ScopeGraph.insertScope scopeAddress scope)

maybeLookupScopePath ::
                ( Member (State (ScopeGraph address)) sig
                , Member (Reader (address, address)) sig
                , Carrier sig m
                , Ord address
                )
             => Declaration
             -> Evaluator term address value m (Maybe (ScopeGraph.Path address))
maybeLookupScopePath Declaration{..} = do
  currentAddress <- currentScope
  scopeGraph <- get
  pure (ScopeGraph.lookupScopePath unDeclaration currentAddress scopeGraph)

lookupScopePath :: ( Member (Resumable (BaseError (ScopeError address))) sig
                , Member (Reader ModuleInfo) sig
                , Member (Reader Span) sig
                , Member (State (ScopeGraph address)) sig
                , Member (Reader (address, address)) sig
                , Carrier sig m
                , Ord address
                )
             => Declaration
             -> Evaluator term address value m (ScopeGraph.Path address)
lookupScopePath decl@Declaration{..} = do
  currentAddress <- currentScope
  scopeGraph <- get
  maybeM (throwScopeError $ LookupPathError decl) (ScopeGraph.lookupScopePath unDeclaration currentAddress scopeGraph)

lookupDeclarationScope :: ( Member (Resumable (BaseError (ScopeError address))) sig
                , Member (Reader ModuleInfo) sig
                , Member (Reader Span) sig
                , Member (State (ScopeGraph address)) sig
                , Member (Reader (address, address)) sig
                , Carrier sig m
                , Ord address
                ) => Declaration -> Evaluator term address value m address
lookupDeclarationScope decl = do
  path <- lookupScopePath decl
  currentScope' <- currentScope
  maybeM (throwScopeError $ LookupDeclarationScopeError decl) (ScopeGraph.pathDeclarationScope currentScope' path)

associatedScope :: (Ord address, Member (State (ScopeGraph address)) sig, Carrier sig m) => Declaration -> Evaluator term address value m (Maybe address)
associatedScope decl = ScopeGraph.associatedScope decl <$> get

withScope :: forall sig m address term value a. ( Carrier sig m
             , Member (Reader (address, address)) sig
             )
          => address
          -> Evaluator term address value m a
          -> Evaluator term address value m a
withScope scope action = local @(address, address) (first (const scope)) action

throwScopeError :: ( Member (Resumable (BaseError (ScopeError address))) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Carrier sig m
                   )
            => ScopeError address resume
            -> Evaluator term address value m resume
throwScopeError = throwBaseError

data ScopeError address return where
  ScopeError :: Declaration -> Span -> ScopeError address (Slot address)
  LookupScopeError :: ScopeError address (Scope address)
  LookupPathError :: Declaration -> ScopeError address (ScopeGraph.Path address)
  LookupDeclarationScopeError :: Declaration -> ScopeError address address
  CurrentScopeError :: ScopeError address address

deriving instance Eq (ScopeError address return)
deriving instance Show (ScopeError address return)
instance Show address => Show1 (ScopeError address) where liftShowsPrec _ _ = showsPrec
instance Eq1 (ScopeError address) where
  liftEq _ (ScopeError m1 n1) (ScopeError m2 n2)           = m1 == m2 && n1 == n2
  liftEq _ CurrentScopeError CurrentScopeError             = True
  liftEq _ LookupScopeError LookupScopeError               = True
  liftEq _ (LookupPathError decl1) (LookupPathError decl2) = decl1 == decl2
  liftEq _ _ _                                             = False

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
