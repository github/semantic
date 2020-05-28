{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The ScopeGraph effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.ScopeGraph
  ( ScopeGraph,
    ScopeGraphEff,
    declare,
    addDeclarations,
    -- Scope Manipulation
    currentScope,
    rootScope,
    putCurrentScope,
    newEdge,
    newReference,
    newScope,
    addBottomScope,
    addTopScope,
    connectScopes,
    withScope,
    declareFunction,
    declareMaybeName,
    declareParameter,
    reference,
    refer,
    Has,
  )
where

import Analysis.Name (Name)
import qualified Analysis.Name as Name
import Control.Algebra
import Control.Effect.Fresh
import Control.Effect.Reader
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props.Reference
import Control.Effect.State
import Control.Lens
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Module as Module
import qualified Data.ScopeGraph as ScopeGraph
import Data.Semilattice.Lower
import Data.Text (Text)
import GHC.Records
import Scope.Graph.AdjacencyList (ScopeGraph)
import qualified Scope.Graph.AdjacencyList as AdjacencyList
import qualified Scope.Reference as Reference
import Scope.Types
import Source.Loc
import Source.Span
import qualified Stack.Graph as Stack

-- | Extract the 'Just' of a 'Maybe' in an 'Applicative' context or, given 'Nothing', run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure
{-# INLINE maybeM #-}

type ScopeGraphEff sig m =
  ( Has (State (ScopeGraph Name)) sig m,
    Has (State (Stack.Graph Stack.Node)) sig m,
    Has (State (CurrentScope Name)) sig m,
    Has (Reader Stack.Node) sig m, -- The root node of the module.
    Has (Reader Module.ModuleInfo) sig m,
    Has Fresh sig m
  )

graphInProgress :: ScopeGraphEff sig m => m (ScopeGraph Name)
graphInProgress = get

currentScope :: ScopeGraphEff sig m => m (CurrentScope Name)
currentScope = get @(CurrentScope Name)

rootScope :: ScopeGraphEff sig m => m Stack.Node
rootScope = ask @Stack.Node

putCurrentScope :: ScopeGraphEff sig m => Name -> m ()
putCurrentScope = put . CurrentScope

withScope ::
  ScopeGraphEff sig m =>
  Name ->
  m a ->
  m a
withScope scope action = do
  CurrentScope s <- get @(CurrentScope Name)
  put (CurrentScope scope)
  x <- action
  put (CurrentScope s)
  pure x

declare :: ScopeGraphEff sig m => Name -> Kind -> Loc -> m Stack.Node
declare n kind loc = do
  let declNode = Stack.Declaration n kind loc
  -- ToDo: generate a unique id for the node in the graph
  pure declNode

refer :: ScopeGraphEff sig m => Name -> Kind -> Loc -> m Stack.Node
refer n kind loc = do
  let nameNode = Stack.Reference n kind loc
  -- ToDo: generate a unique id for the node in the graph
  return nameNode

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
reference :: forall sig m. ScopeGraphEff sig m => Text -> Text -> Props.Reference -> m ()
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

newScope :: forall sig m. ScopeGraphEff sig m => Name -> m Name
newScope currentScope = do
  name <- Name.gensym
  name <$ modify (Stack.newScope name currentScope)

addDeclarations :: ScopeGraphEff sig m => NonEmpty (Name, Kind, Loc) -> m (Stack.Graph Stack.Node)
addDeclarations names = do
  let graph' =
        foldr
          ( \(name, kind, loc) graph ->
              Stack.addEdge (Stack.Declaration name kind loc) (Stack.PopSymbol ".") graph
          )
          mempty
          (NonEmpty.init names)
      graph'' = (Stack.addEdge (Stack.PopSymbol ".") ((\(name, kind, loc) -> (Stack.Declaration name kind loc)) (NonEmpty.last names)) graph')
      graph''' =
        foldr
          ( \(name, kind, loc) graph ->
              Stack.addEdge (Stack.Reference name kind loc) (Stack.PushSymbol ".") graph
          )
          mempty
          (NonEmpty.init $ NonEmpty.reverse names)
      graph'''' = Stack.overlay graph'' (Stack.addEdge (Stack.PushSymbol ".") ((\(name, kind, loc) -> (Stack.Reference name kind loc)) (NonEmpty.head names)) graph''')
      graph''''' = (\(name, kind, loc) -> Stack.addEdge (Stack.Declaration name kind loc) (Stack.Reference name kind loc) graph'''') (NonEmpty.last names)
  pure graph'''''

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

  let refProps = Reference.ReferenceInfo (props ^. span_) (Props.Reference.kind props) lowerBound
      insertRef' :: ScopeGraph.Path Name -> ScopeGraph.ScopeGraph Name -> ScopeGraph.ScopeGraph Name
      insertRef' path scopeGraph =
        let scope' = (ScopeGraph.insertReference (Reference.Reference name) lowerBound (Props.Reference.span props) (getField @"kind" props) path) scope
         in (ScopeGraph.insertScope currentAddress scope' scopeGraph)
  scopeGraph <- get @(ScopeGraph.ScopeGraph Name)
  case AdjacencyList.findPath (const Nothing) (ScopeGraph.Declaration name) currentAddress scopeGraph of
    -- If a path to a declaration is found, insert a reference into the current scope.
    Just path -> modify (insertRef' path)
    -- If no path is found, insert a reference with a hole into the current scope.
    Nothing ->
      modify
        ( ScopeGraph.insertScope
            currentAddress
            ( ScopeGraph.newReference
                (Reference.Reference name)
                refProps
                scope
            )
        )

declareFunction :: forall sig m. ScopeGraphEff sig m => Maybe Name -> Kind -> Loc -> m (Stack.Node, Name)
declareFunction name kind loc = do
  CurrentScope currentScope' <- currentScope
  associatedScope <- newScope currentScope'
  node <- declareMaybeName name kind loc
  pure (node, associatedScope)

declareMaybeName ::
  ScopeGraphEff sig m =>
  Maybe Name ->
  Kind ->
  Loc ->
  m Stack.Node
declareMaybeName maybeName kind loc = do
  case maybeName of
    Just name -> declare name kind loc
    _ -> do
      name <- Name.gensym
      declare name kind loc

declareParameter :: ScopeGraphEff sig m => Name -> Int -> Kind -> Loc -> m Stack.Node
declareParameter n ix kind loc = do
  declNode <- declare n kind loc
  nameNode <- refer n kind loc
  indexNode <- refer (Name.nameI ix) kind loc
  let jumpNode = Stack.JumpToScope
  modify (Stack.addEdge declNode nameNode)
  modify (Stack.addEdge declNode indexNode)
  modify (Stack.addEdge nameNode jumpNode)
  modify (Stack.addEdge indexNode jumpNode)
  pure declNode
