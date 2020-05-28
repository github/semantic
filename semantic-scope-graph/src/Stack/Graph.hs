{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Stack.Graph
  ( Graph (..),
    Node (..),
    Symbol,

    -- * Constructors and glue
    Tagged (..),
    (>>-),
    (-<<),
    singleton,
    fromLinearNodes,

    -- * Reexports
    Class.empty,
    Class.vertex,
    Class.overlay,
    Class.connect,
    Class.edges,
    simplify,
    edgeSet,
    vertexSet,
    removeEdge,
    addEdge,
    transpose,

    -- * Smart constructors
    scope,
    newScope,
    declaration,
    reference,
    popSymbol,
    pushSymbol,
    root,
    topScope,
    bottomScope,

    -- * Predicates
    isRoot,

    -- * Miscellany
    tagGraphUniquely,

    -- * Testing stuff
    testGraph,
    testGraph2,
    edgeTest,
  )
where

import qualified Algebra.Graph as Algebraic
import qualified Algebra.Graph.Class as Class
import qualified Algebra.Graph.ToGraph as ToGraph
import Analysis.Name (Name)
import qualified Analysis.Name as Name
import Control.Applicative
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Control.Lens.Getter
import Control.Monad
import Data.Function
import Data.Functor.Tagged
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Semilattice.Lower
import Data.Set (Set)
import qualified Scope.Types as Scope
import Source.Loc
import Source.Span

type Symbol = Name

data Node
  = Root {symbol :: Symbol}
  | Declaration {symbol :: Symbol, kind :: Scope.Kind, location :: Loc}
  | Reference {symbol :: Symbol, kind :: Scope.Kind, location :: Loc}
  | PushSymbol {symbol :: Symbol}
  | PopSymbol {symbol :: Symbol}
  | PushScope
  | InternalScope {symbol :: Symbol}
  | Scope {symbol :: Symbol}
  | JumpToScope
  | IgnoreScope
  | BottomScope {symbol :: Symbol}
  | TopScope {symbol :: Symbol}
  | InstanceMembers {symbol :: Symbol}
  | ClassMembers {symbol :: Symbol}
  | SelfScope {symbol :: Symbol}
  deriving (Show, Eq, Ord)

-- This overlapping instance is problematic but helps us make sure we don't differentiate two root nodes.
instance {-# OVERLAPS #-} Eq (Tagged Node) where
  x == y = case (view contents y, view contents x) of
    (Root a, Root b) -> a == b
    _ -> view identifier x == view identifier y

instance Ord (Tagged Node) where
  compare = compare `on` view identifier

instance Lower Node where
  lowerBound = Root (Name.nameI 0)

newtype Graph a = Graph {unGraph :: Algebraic.Graph a}
  deriving (Eq, Show)

instance Semigroup (Graph a) where
  (<>) = Class.overlay

instance Monoid (Graph a) where
  mempty = Class.empty

instance Class.Graph (Stack.Graph.Graph a) where
  type Vertex (Stack.Graph.Graph a) = a
  empty = Graph Class.empty
  vertex = Graph . Class.vertex
  overlay (Graph a) (Graph b) = Graph (Class.overlay a b)
  connect (Graph a) (Graph b) = Graph (Class.connect a b)

instance Ord a => ToGraph.ToGraph (Stack.Graph.Graph a) where
  type ToVertex (Stack.Graph.Graph a) = a
  toGraph = ToGraph.toGraph . unGraph

instance Lower a => Lower (Graph a) where
  lowerBound = Graph (Algebraic.vertex lowerBound)

-- | Given @a, b, c@ this returns @a --> b --> c@.
fromLinearNodes :: [a] -> Graph a
fromLinearNodes n = Class.edges $ zip (init n) (drop 1 n)

scope, popSymbol, pushSymbol, topScope, bottomScope :: Symbol -> Graph Node
scope = Class.vertex . Scope
popSymbol = Class.vertex . PopSymbol
pushSymbol = Class.vertex . PushSymbol
topScope = Class.vertex . TopScope
bottomScope = Class.vertex . BottomScope

declaration :: Symbol -> Scope.Kind -> Loc -> Graph Node
declaration symbol kind = Class.vertex . Declaration symbol kind

reference :: Symbol -> Scope.Kind -> Loc -> Graph Node
reference symbol kind = Class.vertex . Reference symbol kind

root :: Name -> Graph Node
root name = Graph (Algebraic.vertex (Root name))

edgeSet :: Ord a => Graph a -> Set (a, a)
edgeSet graph = Algebraic.edgeSet (unGraph graph)

vertexSet :: Ord a => Graph a -> Set a
vertexSet graph = Algebraic.vertexSet (unGraph graph)

tagGraphUniquely :: Graph Node -> Graph (Tagged Node)
tagGraphUniquely =
  simplify
    . run
    . evalFresh 1
    . evalState @(Map Node (Tagged Node)) mempty
    . foldg (pure Class.empty) go (liftA2 Class.overlay) (liftA2 Class.connect)
  where
    go root@Root {} = pure (Class.vertex (root :# 0))
    go n = do
      mSeen <- gets (Map.lookup n)
      vert <- maybeM (taggedM n) mSeen
      when (isNothing mSeen) (modify (Map.insert n vert))
      pure (Class.vertex vert)

foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg a b c d = Algebraic.foldg a b c d . unGraph

(>>-), (-<<) :: Graph a -> Graph a -> Graph a
Graph left >>- Graph right = Graph (Algebraic.connect left right)
(-<<) = flip (>>-)

singleton :: Node -> Graph Node
singleton = Class.vertex

newScope :: Name -> Name -> Graph Node -> Graph Node
newScope name currentScope graph =
  addEdge (Scope name) (Scope currentScope) graph

simplify :: Ord a => Graph a -> Graph a
simplify = Graph . Algebraic.simplify . unGraph

removeEdge :: Ord a => a -> a -> Graph a -> Graph a
removeEdge a b = Graph . Algebraic.removeEdge a b . unGraph

transpose :: Graph a -> Graph a
transpose = Graph . Algebraic.transpose . unGraph

addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge a b = simplify . Graph . Algebraic.overlay (Algebraic.edge a b) . unGraph

maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure

isRoot :: Tagged Node -> Bool
isRoot (node :# _) = case node of
  Root {} -> True
  _ -> False

testEdgeList :: [Node]
testEdgeList =
  [ Scope "current",
    Declaration "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1))),
    PopSymbol "member",
    Declaration "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1))),
    Reference "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1))),
    PushSymbol "member",
    Reference "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1))),
    Root "_a"
  ]

testGraph :: Graph Node
testGraph =
  mconcat
    [ (scope "current" >>- declaration "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (declaration "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1))) >>- popSymbol "member"),
      (popSymbol "member" >>- declaration "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (declaration "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1))) >>- reference "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (reference "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1))) >>- pushSymbol "member"),
      (pushSymbol "member" >>- reference "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (reference "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1))) >>- root "_a")
    ]

testGraph2 :: Graph Node
testGraph2 = fromLinearNodes testEdgeList

edgeTest :: Graph Node
edgeTest =
  Class.edges
    [ (Scope "current", Declaration "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (Declaration "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1))), PopSymbol "member"),
      (PopSymbol "member", Declaration "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (Declaration "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1))), Reference "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (Reference "b" Scope.Identifier (Loc lowerBound (point (Pos 1 1))), PushSymbol "member"),
      (PushSymbol "member", Reference "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1)))),
      (Reference "a" Scope.Identifier (Loc lowerBound (point (Pos 1 1))), Root "_a")
    ]
