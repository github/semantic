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
  , addDeclarations
  -- Scope Manipulation
  , currentScope
  , rootScope
  , putCurrentScope
  , newEdge
  , newReference
  , newScope
  , addBottomScope
  , addTopScope
  , connectScopes
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
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Module as Module
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Records
import qualified Scope.Reference as Reference
import           Source.Loc
import           Source.Span
import           Stack.Graph ((-<<), (>>-))
import qualified Stack.Graph as Stack

import           Scope.Graph.AdjacencyList (ScopeGraph)
import qualified Scope.Graph.AdjacencyList as AdjacencyList
import           Scope.Types

import qualified Control.Effect.ScopeGraph.Properties.Declaration as Props
import qualified Control.Effect.ScopeGraph.Properties.Function as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props.Reference
import           Control.Effect.State

import qualified Algebra.Graph as Graph
import qualified Algebra.Graph.Class as Class

-- | Extract the 'Just' of a 'Maybe' in an 'Applicative' context or, given 'Nothing', run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure
{-# INLINE maybeM #-}

type ScopeGraphEff sig m
  = ( Has (State (ScopeGraph Name)) sig m
    , Has (State (Stack.Graph Stack.Node)) sig m
    , Has (State (CurrentScope Name)) sig m
    , Has (Reader Stack.Node) sig m
    , Has (Reader Module.ModuleInfo) sig m
    , Has Fresh sig m
    )

graphInProgress :: ScopeGraphEff sig m => m (ScopeGraph Name)
graphInProgress = get


currentScope :: ScopeGraphEff sig m => m (CurrentScope Name)
currentScope = get @(CurrentScope Name)

rootScope :: ScopeGraphEff sig m => m Stack.Node
rootScope = ask @Stack.Node

putCurrentScope :: ScopeGraphEff sig m => Name -> m ()
putCurrentScope = put . CurrentScope

withScope :: ScopeGraphEff sig m
          => Name
          -> m a
          -> m a
withScope scope action = do
  CurrentScope s <- get @(CurrentScope Name)
  put (CurrentScope scope)
  x <- action
  put (CurrentScope s)
  pure x

declare :: ScopeGraphEff sig m => Name -> Props.Declaration -> m ()
declare n props = do
  CurrentScope current <- currentScope
  let Props.Declaration kind relation associatedScope span = props
  modify (fst . ScopeGraph.declare (ScopeGraph.Declaration n) (lowerBound @Module.ModuleInfo) relation ScopeGraph.Public span kind associatedScope current)

addBottomScope :: ScopeGraphEff sig m => m Stack.Node
addBottomScope = do
  CurrentScope s <- get @(CurrentScope Name)
  modify (Stack.addEdge (Stack.BottomScope s) (Stack.Scope s))
  pure (Stack.BottomScope s)

addTopScope :: ScopeGraphEff sig m => m Stack.Node
addTopScope = do
  CurrentScope s <- get @(CurrentScope Name)
  modify (Stack.addEdge (Stack.TopScope s) (Stack.Scope s))
  pure (Stack.TopScope s)

connectScopes :: ScopeGraphEff sig m => Stack.Node -> Stack.Node -> m ()
connectScopes scopeA existingScope = do
  modify (Stack.addEdge scopeA existingScope)

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
  name <- Name.gensym
  modify (Stack.newScope name edges)
  name <$ modify (ScopeGraph.newScope name edges)


addDeclarations :: ScopeGraphEff sig m => NonEmpty (Loc, Name) -> m (Stack.Graph Stack.Node)
addDeclarations names = do
  let graph' = foldr (\(_, name) graph ->
        graph -<< (Stack.popSymbol "member") -<< (Stack.declaration name)) mempty (NonEmpty.init names)
      graph'' = graph' >>- (Stack.declaration (snd $ NonEmpty.last names))
      graph''' = foldr (\(_, name) graph ->
        graph -<< (Stack.pushSymbol "member") -<< (Stack.reference name)) mempty (NonEmpty.init $ NonEmpty.reverse names)
      graph'''' = graph'' >>- graph''' >>- (Stack.reference (snd $ NonEmpty.head names))
  pure graph''''

-- | Takes an edge label and a list of names and inserts an import edge to a hole.
newEdge :: ScopeGraphEff sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
newEdge label address = do
  CurrentScope current <- currentScope
  modify (ScopeGraph.addImportEdge label (toList address) current)

lookupScope :: ScopeGraphEff sig m => Name -> m (ScopeGraph.Scope Name)
lookupScope address = maybeM undefined . ScopeGraph.lookupScope address =<< get

-- | Inserts a reference.
newReference :: ScopeGraphEff sig m => Name -> Props.Reference -> m ()
newReference name props = do
  CurrentScope currentAddress <- currentScope
  scope <- lookupScope currentAddress

  let refProps = Reference.ReferenceInfo (props^.span_) (Props.Reference.kind props) lowerBound
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
declareMaybeName maybeName props = do
  case maybeName of
    Just name -> name <$ declare name props
    _         -> do
      name <- Name.gensym
      name <$ declare name (props { Props.relation = ScopeGraph.Gensym })
