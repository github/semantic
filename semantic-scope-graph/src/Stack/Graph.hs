{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLists #-}
module Stack.Graph
  ( Graph(..)
  , Node(..)
  , Symbol
  -- * Constructors and glue
  , (>>-)
  , (-<<)
  , singleton
  , fromLinearNodes
  -- * Reexports
  , Class.empty
  , Class.vertex
  , Class.overlay
  , Class.connect
  , Class.edges
  , simplify
  -- * Smart constructors
  , scope
  , newScope
  , declaration
  , reference
  , popSymbol
  , pushSymbol
  , root
  -- * Testing stuff
  , testGraph
  , testGraph2
  , edgeTest
  ) where

import qualified Algebra.Graph as Algebraic
import qualified Algebra.Graph.Class as Class
import qualified Algebra.Graph.ToGraph as ToGraph
import           Analysis.Name (Name)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semilattice.Lower
import qualified Scope.Types as Scope

type Symbol = Name

data Node = Root
  | Declaration Symbol
  | Reference Symbol
  | PushSymbol Symbol
  | PopSymbol Symbol
  | PushScope
  | Scope Symbol
  | ExportedScope
  | JumpToScope
  | IgnoreScope
  deriving (Show, Eq, Ord)

instance Lower Node where
  lowerBound = Root

newtype Graph a = Graph { unGraph :: Algebraic.Graph a }
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

scope, declaration, popSymbol, reference, pushSymbol :: Symbol -> Graph Node
scope = Class.vertex . Scope
declaration = Class.vertex . Declaration
reference = Class.vertex . Reference
popSymbol = Class.vertex . PopSymbol
pushSymbol = Class.vertex . PushSymbol

root :: Graph Node
root = Graph (Algebraic.vertex Root)


(>>-), (-<<) :: Graph a -> Graph a -> Graph a
Graph left >>- Graph right = Graph (Algebraic.connect left right)
(-<<) = flip (>>-)

singleton :: Node -> Graph Node
singleton = Class.vertex

newScope :: Name -> Map Scope.EdgeLabel [Name] -> Graph Node -> Graph Node
newScope name edges graph =
  Map.foldrWithKey (\_ scopes graph ->
    foldr (\scope' graph ->
      simplify $ Class.overlay (Class.edges [(Scope name, Scope scope')]) (graph))
      graph scopes) graph edges

simplify :: Ord a => Graph a -> Graph a
simplify = Graph . Algebraic.simplify . unGraph

testEdgeList :: [Node]
testEdgeList =
  [ Scope "current"
  , Declaration "a"
  , PopSymbol "member"
  , Declaration "b"
  , Reference "b"
  , PushSymbol "member"
  , Reference "a"
  , Root
  ]

testGraph :: Graph Node
testGraph = mconcat
  [ (scope "current" >>- (declaration "a" >>- popSymbol "member"))
  , (popSymbol "member" >>- declaration "b")
  , (declaration "b" >>- reference "b")
  , (reference "b" >>- pushSymbol "member")
  , (pushSymbol "member" >>- reference "a")
  , (reference "a" >>- root)
  ]

testGraph2 :: Graph Node
testGraph2 = fromLinearNodes testEdgeList

edgeTest :: Graph Node
edgeTest = Class.edges
  [ (Scope "current" , Declaration "a")
  , (Declaration "a" , PopSymbol "member")
  , (PopSymbol "member" , Declaration "b")
  , (Declaration "b" , Reference "b")
  , (Reference "b" , PushSymbol "member")
  , (PushSymbol "member" , Reference "a")
  , (Reference "a" , Root)
  ]
