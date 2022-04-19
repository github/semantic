{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The ScopeGraph effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.ScopeGraph
  ( ScopeGraph
  , ScopeGraphEff
  , declare
  -- Scope Manipulation
  , currentScope
  , newEdge
  , newReference
  , newScope
  , withScope
  , declareFunction
  , declareMaybeName
  , reference
  , Has
  ) where

import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import           Control.Algebra
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Lens
import           Data.List.NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Module as Module
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import           Data.Text (Text)
import           GHC.Records
import qualified Scope.Reference as Reference
import           Source.Span

import           Scope.Graph.AdjacencyList (ScopeGraph)
import qualified Scope.Graph.AdjacencyList as AdjacencyList
import           Scope.Types

import qualified Control.Effect.ScopeGraph.Properties.Declaration as Props
import qualified Control.Effect.ScopeGraph.Properties.Function as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props.Reference
import           Control.Effect.State

-- | Extract the 'Just' of a 'Maybe' in an 'Applicative' context or, given 'Nothing', run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure
{-# INLINE maybeM #-}

type ScopeGraphEff sig m
  = ( Has (State (ScopeGraph Name)) sig m
    , Has (State Name) sig m
    , Has (Reader Module.ModuleInfo) sig m
    , Has Fresh sig m
    , Has (Reader (CurrentScope Name)) sig m
    , Has (Reader Module.ModuleInfo) sig m
    )

graphInProgress :: ScopeGraphEff sig m => m (ScopeGraph Name)
graphInProgress = get

currentScope :: ScopeGraphEff sig m => m (CurrentScope Name)
currentScope = ask

withScope :: ScopeGraphEff sig m
          => CurrentScope Name
          -> m a
          -> m a
withScope scope = local (const scope)


declare :: ScopeGraphEff sig m => Name -> Props.Declaration -> m ()
declare n props = do
  CurrentScope current <- currentScope
  old <- graphInProgress
  info <- ask
  let Props.Declaration kind relation associatedScope span = props
  let (new, _pos) =
         ScopeGraph.declare
         (ScopeGraph.Declaration n)
         info
         relation
         ScopeGraph.Public
         span
         kind
         associatedScope
         current
         old
  put new

-- | Establish a reference to a prior declaration.
reference :: forall sig m . ScopeGraphEff sig m => Text -> Text -> Props.Reference -> m ()
reference n decl props = do
  CurrentScope current <- currentScope
  old <- graphInProgress
  info <- ask
  let new =
         ScopeGraph.reference
         (ScopeGraph.Reference (Name.name n))
         info
         (Props.Reference.span props)
         (Props.Reference.kind props)
         (ScopeGraph.Declaration (Name.name decl))
         current
         old
  put new

newScope :: forall sig m . ScopeGraphEff sig m => Map ScopeGraph.EdgeLabel [Name] -> m Name
newScope edges = do
  old <- graphInProgress
  name <- Name.gensym
  let new = ScopeGraph.newScope name edges old
  name <$ put new

-- | Takes an edge label and a list of names and inserts an import edge to a hole.
newEdge :: ScopeGraphEff sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
newEdge label address = do
  CurrentScope current <- currentScope
  old <- graphInProgress
  let new = ScopeGraph.addImportEdge label (toList address) current old
  put new

lookupScope :: ScopeGraphEff sig m => Name -> m (ScopeGraph.Scope Name)
lookupScope address = maybeM undefined . ScopeGraph.lookupScope address =<< get

-- | Inserts a reference.
newReference :: ScopeGraphEff sig m => Name -> Props.Reference -> m ()
newReference name props = do
  CurrentScope currentAddress <- currentScope
  scope <- lookupScope currentAddress

  let refProps = Reference.ReferenceInfo (props ^. span_) (Props.Reference.kind props) lowerBound
      insertRef' :: ScopeGraph.Path Name -> ScopeGraph.ScopeGraph Name -> ScopeGraph.ScopeGraph Name
      insertRef' path scopeGraph = let
          scope' = (ScopeGraph.insertReference (Reference.Reference name) lowerBound (Props.Reference.span props) (getField @"kind" props) path) scope
        in
          (ScopeGraph.insertScope currentAddress scope' scopeGraph)
  scopeGraph <- get @(ScopeGraph.ScopeGraph Name)
  case AdjacencyList.findPath (const Nothing) (ScopeGraph.Declaration name) currentAddress scopeGraph of
    -- If a path to a declaration is found, insert a reference into the current scope.
    Just path -> modify (insertRef' path)
    -- If no path is found, insert a reference with a hole into the current scope.
    Nothing   ->
      modify (ScopeGraph.insertScope
              currentAddress
              (ScopeGraph.newReference
                (Reference.Reference name)
                refProps
                scope))

declareFunction :: forall sig m . ScopeGraphEff sig m => Maybe Name -> Props.Function -> m (Name, Name)
declareFunction name (Props.Function kind span) = do
  CurrentScope currentScope' <- currentScope
  let lexicalEdges = Map.singleton ScopeGraph.Lexical [ currentScope' ]
  associatedScope <- newScope lexicalEdges
  name' <- declareMaybeName name Props.Declaration
                                   { Props.relation = ScopeGraph.Default
                                   , Props.kind = kind
                                   , Props.associatedScope = Just associatedScope
                                   , Props.span = span
                                   }
  pure (name', associatedScope)

declareMaybeName :: ScopeGraphEff sig m
                 => Maybe Name
                 -> Props.Declaration
                 -> m Name
declareMaybeName maybeName props@(Props.Declaration kind _ associatedScope span) = do
  case maybeName of
    Just name -> name <$ declare name props
    _         -> do
      name <- Name.gensym
      name <$ declare name (Props.Declaration kind ScopeGraph.Gensym associatedScope span)
