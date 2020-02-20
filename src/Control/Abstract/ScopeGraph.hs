{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Abstract.ScopeGraph
  ( lookup
  , declare
  , declareMaybeName
  , reference
  , newScope
  , newPreludeScope
  , Declaration(..)
  , ScopeGraph
  , ScopeError(..)
  , Reference(..)
  , Relation(..)
  , EdgeLabel(..)
  , CurrentScope(..)
  , Info(..)
  , AccessControl(..)
  , currentScope
  , insertExportEdge
  , insertImportEdge
  , insertLexicalEdge
  , withScope
  , associatedScope
  , declarationByName
  , declarationsByAccessControl
  , declarationsByRelation
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

import           Analysis.Name hiding (name)
import           Control.Abstract.Evaluator hiding (Local)
import           Control.Algebra
import qualified Control.Carrier.Resumable.Either as Either
import qualified Control.Carrier.Resumable.Resume as With
import           Data.Abstract.BaseError
import           Data.Abstract.Module
import           Data.Abstract.ScopeGraph
    ( AccessControl (..)
    , Declaration (..)
    , EdgeLabel
    , Info (..)
    , Kind
    , Reference
    , Relation (..)
    , Scope (..)
    , ScopeGraph
    , Slot (..)
    )
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Functor.Classes
import           Data.Map (Map)
import           Data.Maybe.Exts
import           GHC.Generics (Generic1)
import           Prelude hiding (lookup)
import           Scope.Types (CurrentScope (..))
import           Source.Span

lookup :: ( Ord address
          , Has (State (ScopeGraph address)) sig m
          )
       => Reference
       -> Evaluator term address value m (Maybe address)
lookup ref = ScopeGraph.scopeOfRef ref <$> get

declare :: ( Has (State (ScopeGraph address)) sig m
           , Has (Reader (CurrentScope address)) sig m
           , Has (Reader ModuleInfo) sig m
           , Ord address
           )
        => Declaration
        -> Relation
        -> AccessControl
        -> Span
        -> Kind
        -> Maybe address
        -> Evaluator term address value m ()
declare decl rel accessControl span kind scope = do
  currentAddress <- currentScope
  moduleInfo <- ask @ModuleInfo
  modify (fst . ScopeGraph.declare decl moduleInfo rel accessControl span kind scope currentAddress)

-- | If the provided name is 'Nothing' we want to reflect that the declaration's name was a generated name (gensym).
-- We use the 'Gensym' relation to indicate that. Otherwise, we use the provided 'relation'.
declareMaybeName :: ( Has (State (ScopeGraph address)) sig m
                    , Has (Reader (CurrentScope address)) sig m
                    , Has (Reader ModuleInfo) sig m
                    , Has Fresh sig m
                    , Ord address
                    )
                 => Maybe Name
                 -> Relation
                 -> AccessControl
                 -> Span
                 -> Kind
                 -> Maybe address
                 -> Evaluator term address value m Name
declareMaybeName maybeName relation ac span kind scope = do
  case maybeName of
    Just name -> declare (Declaration name) relation ac span kind scope >> pure name
    _         -> gensym >>= \name -> declare (Declaration name) Gensym ac span kind scope >> pure name

putDeclarationScope :: ( Ord address
                       , Has (Reader (CurrentScope address)) sig m
                       , Has (State (ScopeGraph address)) sig m
                       )
                     => Declaration
                     -> address
                     -> Evaluator term address value m ()
putDeclarationScope decl assocScope = do
  currentAddress <- currentScope
  modify (ScopeGraph.insertDeclarationScope decl assocScope currentAddress)

putDeclarationSpan :: forall address sig m term value .
                      ( Ord address
                      , Has (State (ScopeGraph address)) sig m
                      )
                   => Declaration
                   -> Span
                   -> Evaluator term address value m ()
putDeclarationSpan decl = modify @(ScopeGraph address) . ScopeGraph.insertDeclarationSpan decl

reference :: forall address sig m term value .
             ( Ord address
             , Has (State (ScopeGraph address)) sig m
             , Has (Reader (CurrentScope address)) sig m
             , Has (Reader ModuleInfo) sig m
             )
          => Reference
          -> Span
          -> Kind
          -> Declaration
          -> Evaluator term address value m ()
reference ref span kind decl = do
  currentAddress <- currentScope
  moduleInfo <- ask @ModuleInfo
  modify @(ScopeGraph address) (ScopeGraph.reference ref moduleInfo span kind decl currentAddress)

-- | Combinator to insert an export edge from the current scope to the provided scope address.
insertExportEdge :: (Has (Reader (CurrentScope scopeAddress)) sig m, Has (State (ScopeGraph scopeAddress)) sig m, Ord scopeAddress)
                 => scopeAddress
                 -> Evaluator term scopeAddress value m ()
insertExportEdge = insertEdge ScopeGraph.Export

-- | Combinator to insert an import edge from the current scope to the provided scope address.
insertImportEdge :: (Has (Reader (CurrentScope scopeAddress)) sig m, Has (State (ScopeGraph scopeAddress)) sig m, Ord scopeAddress)
                 => scopeAddress
                 -> Evaluator term scopeAddress value m ()
insertImportEdge = insertEdge ScopeGraph.Import

-- | Combinator to insert a lexical edge from the current scope to the provided scope address.
insertLexicalEdge :: (Has (Reader (CurrentScope scopeAddress)) sig m, Has (State (ScopeGraph scopeAddress)) sig m, Ord scopeAddress)
                  => scopeAddress
                  -> Evaluator term scopeAddress value m ()
insertLexicalEdge = insertEdge ScopeGraph.Lexical

insertEdge :: ( Has (State (ScopeGraph address)) sig m
              , Has (Reader (CurrentScope address)) sig m
              , Ord address)
           => EdgeLabel
           -> address
           -> Evaluator term address value m ()
insertEdge label target = do
  currentAddress <- currentScope
  modify (ScopeGraph.insertEdge label target currentAddress)

-- | Inserts a new scope into the scope graph with the given edges.
newScope :: ( Has (Allocator address) sig m
            , Has (State (ScopeGraph address)) sig m
            , Has Fresh sig m
            , Ord address
            )
         => Map EdgeLabel [address]
         -> Evaluator term address value m address
newScope edges = do
  -- Take the edges and construct a new scope
  name <- gensym
  address <- alloc name
  address <$ modify (ScopeGraph.newScope address edges)

-- | Inserts a new scope into the scope graph with the given edges.
newPreludeScope :: ( Has (Allocator address) sig m
            , Has (State (ScopeGraph address)) sig m
            , Has Fresh sig m
            , Ord address
            )
         => Map EdgeLabel [address]
         -> Evaluator term address value m address
newPreludeScope edges = do
  -- Take the edges and construct a new scope
  name <- gensym
  address <- alloc name
  address <$ modify (ScopeGraph.newPreludeScope address edges)

currentScope :: Has (Reader (CurrentScope address)) sig m
             => Evaluator term address value m address
currentScope = asks unCurrentScope

lookupScope :: ( Has (Resumable (BaseError (ScopeError address))) sig m
               , Has (Reader ModuleInfo) sig m
               , Has (Reader Span) sig m
               , Has (State (ScopeGraph address)) sig m
               , Ord address
               )
            => address
            -> Evaluator term address value m (Scope address)
lookupScope address = maybeM (throwScopeError LookupScopeError) . ScopeGraph.lookupScope address =<< get

declarationsByRelation :: ( Has (State (ScopeGraph address)) sig m
                          , Ord address
                          )
                       => address
                       -> Relation
                       -> Evaluator term address value m [ Info address ]
declarationsByRelation scope relation = ScopeGraph.declarationsByRelation scope relation <$> get

declarationByName :: ( Has (Resumable (BaseError (ScopeError address))) sig m
                     , Has (Reader ModuleInfo) sig m
                     , Has (Reader Span) sig m
                     , Has (State (ScopeGraph address)) sig m
                     , Ord address
                     )
                  => address
                  -> Declaration
                  -> Evaluator term address value m (Info address)
declarationByName scope name = do
  scopeGraph <- get
  maybeM (throwScopeError $ DeclarationByNameError name) (ScopeGraph.declarationByName scope name scopeGraph)

declarationsByAccessControl :: ( Has (State (ScopeGraph address)) sig m
                               , Ord address
                               )
                            => address
                            -> AccessControl
                            -> Evaluator term address value m [ Info address ]
declarationsByAccessControl scopeAddress accessControl = ScopeGraph.declarationsByAccessControl scopeAddress accessControl <$> get

insertImportReference :: ( Has (Resumable (BaseError (ScopeError address))) sig m
                        , Has (Reader ModuleInfo) sig m
                        , Has (Reader Span) sig m
                        , Has (State (ScopeGraph address)) sig m
                        , Has (Reader (CurrentScope address)) sig m
                        , Ord address
                        )
                      => Reference
                      -> Span
                      -> Kind
                      -> Declaration
                      -> address
                      -> Evaluator term address value m ()
insertImportReference ref span kind decl scopeAddress = do
  scopeGraph <- get
  scope <- lookupScope scopeAddress
  currentAddress <- currentScope
  moduleInfo <- ask @ModuleInfo
  newScope <- maybeM (throwScopeError ImportReferenceError) (ScopeGraph.insertImportReference ref moduleInfo span kind decl currentAddress scopeGraph scope)
  insertScope scopeAddress newScope

insertScope :: ( Has (State (ScopeGraph address)) sig m
               , Ord address
               )
            => address
            -> Scope address
            -> Evaluator term address value m ()
insertScope scopeAddress scope = modify (ScopeGraph.insertScope scopeAddress scope)

maybeLookupScopePath :: ( Has (State (ScopeGraph address)) sig m
                        , Has (Reader (CurrentScope address)) sig m
                        , Ord address
                        )
                     => Declaration
                     -> Evaluator term address value m (Maybe (ScopeGraph.Path address))
maybeLookupScopePath Declaration{..} = do
  currentAddress <- currentScope
  gets (ScopeGraph.lookupScopePath unDeclaration currentAddress)

lookupScopePath :: ( Has (Resumable (BaseError (ScopeError address))) sig m
                   , Has (Reader ModuleInfo) sig m
                   , Has (Reader Span) sig m
                   , Has (State (ScopeGraph address)) sig m
                   , Has (Reader (CurrentScope address)) sig m
                   , Ord address
                   )
                => Declaration
                -> Evaluator term address value m (ScopeGraph.Path address)
lookupScopePath decl@Declaration{..} = do
  currentAddress <- currentScope
  scopeGraph <- get
  maybeM (throwScopeError $ LookupPathError decl) (ScopeGraph.lookupScopePath unDeclaration currentAddress scopeGraph)

lookupDeclarationScope :: ( Has (Resumable (BaseError (ScopeError address))) sig m
                          , Has (Reader ModuleInfo) sig m
                          , Has (Reader Span) sig m
                          , Has (State (ScopeGraph address)) sig m
                          , Has (Reader (CurrentScope address)) sig m
                          , Ord address
                          )
                       => Declaration
                       -> Evaluator term address value m address
lookupDeclarationScope decl = do
  path <- lookupScopePath decl
  currentScope' <- currentScope
  maybeM (throwScopeError $ LookupDeclarationScopeError decl) (ScopeGraph.pathDeclarationScope currentScope' path)

associatedScope :: (Ord address, Has (State (ScopeGraph address)) sig m) => Declaration -> Evaluator term address value m (Maybe address)
associatedScope decl = ScopeGraph.associatedScope decl <$> get

withScope :: Has (Reader (CurrentScope address)) sig m
          => address
          -> Evaluator term address value m a
          -> Evaluator term address value m a
withScope scope = local (const (CurrentScope scope))

throwScopeError :: ( Has (Resumable (BaseError (ScopeError address))) sig m
                   , Has (Reader ModuleInfo) sig m
                   , Has (Reader Span) sig m
                   )
                => ScopeError address resume
                -> Evaluator term address value m resume
throwScopeError = throwBaseError

data ScopeError address return where
  ScopeError :: Declaration -> Span -> ScopeError address (Slot address)
  LookupScopeError :: ScopeError address (Scope address)
  ImportReferenceError :: ScopeError address (Scope address)
  LookupPathError :: Declaration -> ScopeError address (ScopeGraph.Path address)
  LookupDeclarationScopeError :: Declaration -> ScopeError address address
  DeclarationByNameError :: Declaration -> ScopeError address (Info address)
  CurrentScopeError :: ScopeError address address

deriving instance Eq (ScopeError address return)
deriving instance Show (ScopeError address return)
instance Show address => Show1 (ScopeError address) where liftShowsPrec _ _ = showsPrec
instance Eq1 (ScopeError address) where
  liftEq _ (ScopeError m1 n1)                   (ScopeError m2 n2)                  = m1 == m2 && n1 == n2
  liftEq _ LookupScopeError                     LookupScopeError                    = True
  liftEq _ ImportReferenceError                 ImportReferenceError                = True
  liftEq _ (LookupPathError decl1)              (LookupPathError decl2)             = decl1 == decl2
  liftEq _ (LookupDeclarationScopeError decl1)  (LookupDeclarationScopeError decl2) = decl1 ==  decl2
  liftEq _ CurrentScopeError                    CurrentScopeError                   = True
  liftEq _ _                                    _                                   = False

alloc :: (Has (Allocator address) sig m) => Name -> Evaluator term address value m address
alloc = send . flip Alloc pure

data Allocator address (m :: * -> *) k
  = Alloc Name (address -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Allocator address)
instance Effect   (Allocator address)

runAllocator :: Evaluator term address value (AllocatorC address m) a
             -> Evaluator term address value m a
runAllocator = raiseHandler runAllocatorC

newtype AllocatorC address m a = AllocatorC { runAllocatorC :: m a }
  deriving (Alternative, Applicative, Functor, Monad)

runScopeErrorWith :: (forall resume . BaseError (ScopeError address) resume -> Evaluator term address value m resume)
                  -> Evaluator term address value (With.ResumableC (BaseError (ScopeError address)) m) a
                  -> Evaluator term address value m a
runScopeErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)

runScopeError :: Evaluator term address value (Either.ResumableC (BaseError (ScopeError address)) m) a
              -> Evaluator term address value m (Either (Either.SomeError (BaseError (ScopeError address))) a)
runScopeError = raiseHandler Either.runResumable
