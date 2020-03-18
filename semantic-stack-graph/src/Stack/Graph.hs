{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Stack.Graph
  ( Graph(..)
  , Node(..)
  , Symbol
  -- * Constructors and glue
  , Tagged (..)
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
  , edgeSet
  , removeEdge
  -- * Smart constructors
  , scope
  , newScope
  , declaration
  , reference
  , popSymbol
  , pushSymbol
  , root
  -- * Miscellany
  , tagGraphUniquely
  -- * Testing stuff
  , testGraph
  , testGraph2
  , edgeTest
  ) where

import qualified Algebra.Graph as Algebraic
import qualified Algebra.Graph.Class as Class
import qualified Algebra.Graph.ToGraph as ToGraph
import           Analysis.Name (Name)
import           Control.Applicative
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import           Control.Lens.Getter
import           Control.Monad
import           Data.Function
import           Data.Functor.Tagged
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Semilattice.Lower
import           Data.Set (Set)
import qualified Data.Set as Set
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

-- This overlapping instance is problematic but helps us make sure we don't differentiate two root nodes.
instance {-# OVERLAPS #-} Eq (Tagged Node) where
  x == y = case (view contents y, view contents x) of
    (Root, Root) -> True
    _            -> view identifier x == view identifier y

instance Ord (Tagged Node) where
  compare = compare `on` view identifier

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

edgeSet :: Graph Node -> Set (Node, Node)
edgeSet graph = Algebraic.edgeSet (unGraph graph)

tagGraphUniquely :: Graph Node -> Graph (Tagged Node)
tagGraphUniquely
  = simplify
  . run
  . evalFresh 0
  . evalState @(Map Node (Tagged Node)) mempty
  . foldg (pure Class.empty) go (liftA2 Class.overlay) (liftA2 Class.connect)
    where
      go n = do
        mSeen <- gets (Map.lookup n)
        vert  <- maybeM (taggedM n) mSeen
        when (isNothing mSeen) (modify (Map.insert n vert))
        pure (Class.vertex vert)

foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg a b c d = Algebraic.foldg a b c d . unGraph

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

removeEdge :: Ord a => a -> a -> Graph a -> Graph a
removeEdge a b = Graph . Algebraic.removeEdge a b . unGraph

maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure

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
  [ (scope "current" >>- declaration "a")
  , (declaration "a" >>- popSymbol "member")
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
