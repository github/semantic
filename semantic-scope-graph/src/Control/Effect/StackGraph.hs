{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The ScopeGraph effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.StackGraph
  ( ScopeGraph,
    StackGraphEff,
    declare,
    addDeclarations,
    -- Scope Manipulation
    currentScope,
    rootScope,
    putCurrentScope,
    pushSymbol,
    popSymbol,
    newScope,
    selfScope,
    scope,
    internalScope,
    instanceMemberScope,
    classMemberScope,
    addBottomScope,
    addTopScope,
    withScope,
    declareFunction,
    declareMaybeName,
    declareParameter,
    refer,
    Has,
    ensureAST,
    ParseError (..),
    Tagged,
  )
where

import qualified AST.Parse as Parse
import Analysis.Name (Name)
import qualified Analysis.Name as Name
import Control.Algebra
import Control.Effect.Error
import Control.Effect.Exception
import Control.Effect.Fresh
import Control.Effect.Labelled
import Control.Effect.Reader
import Control.Effect.State
import Control.Lens
import Data.Foldable
import Data.Functor.Tagged hiding (taggedM)
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Module as Module
import qualified Data.ScopeGraph as ScopeGraph
import Data.Semilattice.Lower
import Data.Text (Text)
import GHC.Records
import qualified Proto.Semantic as P
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

type StackGraphEff sig m =
  ( Has (State (Stack.Graph (Tagged Stack.Node))) sig m,
    Has (State (CurrentScope (Tagged Stack.Node))) sig m,
    Has (Reader (Tagged Stack.Node)) sig m, -- The root node of the module.
    Has (Reader Module.ModuleInfo) sig m,
    Has (Throw ParseError) sig m,
    Has Fresh sig m, -- gensyming names for anonymous functions
    HasLabelled Tagged Fresh sig m
  )

newtype ParseError = ParseError {unParseError :: String}
  deriving (Eq, Ord, Show, Exception)

ensureAST :: StackGraphEff sig m => Parse.Err a -> m a
ensureAST (Parse.Fail error) = throwError (ParseError error)
ensureAST (Parse.Success term) = pure term

currentScope :: StackGraphEff sig m => m (CurrentScope (Tagged Stack.Node))
currentScope = get @(CurrentScope (Tagged Stack.Node))

rootScope :: StackGraphEff sig m => m (Tagged Stack.Node)
rootScope = ask @(Tagged Stack.Node)

putCurrentScope :: StackGraphEff sig m => Tagged Stack.Node -> m ()
putCurrentScope = put . CurrentScope

withScope ::
  StackGraphEff sig m =>
  Tagged Stack.Node ->
  m a ->
  m a
withScope scope action = do
  CurrentScope s <- get @(CurrentScope (Tagged Stack.Node))
  put (CurrentScope scope)
  x <- action
  put (CurrentScope s)
  pure x

declare :: StackGraphEff sig m => Name -> P.SyntaxType -> Loc -> m (Tagged Stack.Node)
declare n kind loc = do
  let declNode = Stack.Declaration n kind loc
  taggedM declNode

refer :: StackGraphEff sig m => Name -> P.SyntaxType -> Loc -> m (Tagged Stack.Node)
refer n kind loc = do
  let nameNode = Stack.Reference n kind loc
  taggedM nameNode

jumpTo :: StackGraphEff sig m => m (Tagged Stack.Node)
jumpTo = taggedM Stack.JumpToScope

bottomScope :: StackGraphEff sig m => Tagged Stack.Node -> m (Tagged Stack.Node)
bottomScope = taggedM . Stack.BottomScope

topScope :: StackGraphEff sig m => Tagged Stack.Node -> m (Tagged Stack.Node)
topScope = taggedM . Stack.BottomScope

scope :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
scope = taggedM . Stack.Scope

pushSymbol :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
pushSymbol = taggedM . Stack.PushSymbol

popSymbol :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
popSymbol = taggedM . Stack.PopSymbol

selfScope :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
selfScope = taggedM . Stack.SelfScope

instanceMemberScope :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
instanceMemberScope = taggedM . Stack.InstanceMembers

classMemberScope :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
classMemberScope = taggedM . Stack.ClassMembers

internalScope :: StackGraphEff sig m => Name -> m (Tagged Stack.Node)
internalScope = taggedM . Stack.InternalScope

taggedM :: HasLabelled Tagged Fresh sig m => a -> m (Tagged a)
taggedM a = (a :#) . fromIntegral <$> runUnderLabel @Tagged fresh

addBottomScope :: StackGraphEff sig m => m (Tagged Stack.Node)
addBottomScope = do
  CurrentScope s <- get @(CurrentScope (Tagged Stack.Node))
  bottomScopeNode <- bottomScope s
  modify (Stack.addEdge bottomScopeNode s)
  pure bottomScopeNode

addTopScope :: StackGraphEff sig m => m (Tagged Stack.Node)
addTopScope = do
  CurrentScope s <- get @(CurrentScope (Tagged Stack.Node))
  topScopeNode <- topScope s
  modify (Stack.addEdge topScopeNode s)
  pure topScopeNode

newScope :: forall sig m. StackGraphEff sig m => Tagged Stack.Node -> m (Tagged Stack.Node)
newScope currentScope = do
  name <- Name.gensym
  scope' <- scope name
  scope' <$ modify (Stack.addEdge scope' currentScope)

addDeclarations :: StackGraphEff sig m => NonEmpty (Name, P.SyntaxType, Loc) -> m (Stack.Graph (Tagged Stack.Node), Tagged Stack.Node)
addDeclarations names = do
  -- Given foo.bar.baz or baz
  -- Construct Declaration foo . Declaration bar or Nothing
  (graph', maybeLastMemberNode) <-
    foldrM
      ( \(name, kind, loc) (graph, _) -> do
          declaration <- declare name kind loc
          memberNode <- popSymbol "."
          pure (Stack.addEdge declaration memberNode graph, Just memberNode)
      )
      (mempty, Nothing)
      (NonEmpty.init names)

  -- If there was a lastMemberNode, construct an edge from it to baz.
  lastDeclaration <- ((\(name, kind, loc) -> (declare name kind loc)) (NonEmpty.last names))
  let graph'' = maybe graph' (\lastMemberNode -> Stack.addEdge lastMemberNode lastDeclaration graph') maybeLastMemberNode

  -- Given foo.bar.baz
  -- Construct references too foo.bar.baz
  references <- mapM (\(name, kind, loc) -> refer name kind loc) (NonEmpty.reverse names)

  -- Construct a Reference baz . Reference bar graph
  (graph''', maybeInitMemberNode) <-
    foldrM
      ( \reference (graph, _) -> do
          memberNode <- popSymbol "."
          pure (Stack.addEdge reference memberNode graph, Just memberNode)
      )
      (mempty, Nothing)
      (NonEmpty.init references)

  -- Let initialReference = foo
  -- Construct Reference baz . Reference bar . Reference foo and overlay it onto graph'' if maybeInitMemberNode exists.
  let initialReference = (NonEmpty.last references)
  let graph'''' = maybe graph'' (\initMemberNode -> Stack.overlay graph'' (Stack.addEdge initMemberNode initialReference graph''')) maybeInitMemberNode

  -- Construct an edge between the last declaration and its corresponding reference
  let lastReference = NonEmpty.head references
  let graph''''' = Stack.addEdge lastDeclaration lastReference graph''''
  pure (graph''''', initialReference)

-- | Takes an edge label and a list of names and inserts an import edge to a hole.
-- newEdge :: StackGraphEff sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
-- newEdge label address = do
--   CurrentScope current <- currentScope
-- modify (ScopeGraph.addImportEdge label (toList address) current)
-- lookupScope :: StackGraphEff sig m => Name -> m (ScopeGraph.Scope Name)
-- lookupScope address = maybeM undefined . ScopeGraph.lookupScope address =<< get

-- | Inserts a reference.
-- newReference :: StackGraphEff sig m => Name -> Props.Reference -> m ()
-- newReference name props = do
--   CurrentScope currentAddress <- currentScope
--   scope <- lookupScope currentAddress

-- let refProps = Reference.ReferenceInfo (props ^. span_) (Props.Reference.kind props) lowerBound
--     insertRef' :: ScopeGraph.Path Name -> ScopeGraph.ScopeGraph Name -> ScopeGraph.ScopeGraph Name
--     insertRef' path scopeGraph =
--       let scope' = (ScopeGraph.insertReference (Reference.Reference name) lowerBound (Props.Reference.span props) (getField @"kind" props) path) scope
--        in (ScopeGraph.insertScope currentAddress scope' scopeGraph)
-- scopeGraph <- get @(ScopeGraph.ScopeGraph Name)
-- case AdjacencyList.findPath (const Nothing) (ScopeGraph.Declaration name) currentAddress scopeGraph of
--   -- If a path to a declaration is found, insert a reference into the current scope.
--   Just path -> modify (insertRef' path)
--   -- If no path is found, insert a reference with a hole into the current scope.
--   Nothing ->
--     modify
--       ( ScopeGraph.insertScope
--           currentAddress
--           ( ScopeGraph.newReference
--               (Reference.Reference name)
--               refProps
--               scope
--           )
--       )

declareFunction :: forall sig m. StackGraphEff sig m => Maybe Name -> P.SyntaxType -> Loc -> m (Tagged Stack.Node, Tagged Stack.Node)
declareFunction name kind loc = do
  CurrentScope currentScope' <- currentScope
  associatedScope <- newScope currentScope'
  node <- declareMaybeName name kind loc
  pure (node, associatedScope)

declareMaybeName ::
  StackGraphEff sig m =>
  Maybe Name ->
  P.SyntaxType ->
  Loc ->
  m (Tagged Stack.Node)
declareMaybeName maybeName kind loc = do
  case maybeName of
    Just name -> declare name kind loc
    _ -> do
      name <- Name.gensym
      declare name kind loc

declareParameter :: StackGraphEff sig m => Name -> Int -> P.SyntaxType -> Loc -> m (Tagged Stack.Node)
declareParameter n ix kind loc = do
  declNode <- declare n kind loc
  nameNode <- refer n kind loc
  indexNode <- refer (Name.nameI ix) kind loc
  jumpNode <- jumpTo
  modify (Stack.addEdge declNode nameNode)
  modify (Stack.addEdge declNode indexNode)
  modify (Stack.addEdge nameNode jumpNode)
  modify (Stack.addEdge indexNode jumpNode)
  pure declNode
