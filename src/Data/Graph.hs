{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Data.Graph
( Graph(..)
, overlay
, connect
, vertex
, Lower(..)
, simplify
, topologicalSort
, JSONVertex(..)
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.AdjacencyMap as A
import Algebra.Graph.Class (connect, overlay, vertex)
import qualified Algebra.Graph.Class as Class
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Aeson
import qualified Data.Set as Set
import Prologue

-- | An algebraic graph with 'Ord', 'Semigroup', and 'Monoid' instances.
newtype Graph vertex = Graph { unGraph :: G.Graph vertex }
  deriving (Alternative, Applicative, Eq, Foldable, Functor, Class.Graph, Monad, Show, Class.ToGraph, Traversable)


simplify :: Ord vertex => Graph vertex -> Graph vertex
simplify (Graph graph) = Graph (G.simplify graph)


-- | Sort a graph’s vertices topologically.
--
-- >>> topologicalSort (Class.path "ab")
-- "ba"
--
-- >>> topologicalSort (Class.path "abc")
-- "cba"
--
-- >>> topologicalSort ((vertex 'a' `connect` vertex 'b') `connect` vertex 'c')
-- "cba"
--
-- >>> topologicalSort (vertex 'a' `connect` (vertex 'b' `connect` vertex 'c'))
-- "cba"
--
-- >>> topologicalSort ((vertex 'a' `connect` vertex 'b') <> (vertex 'a' `connect` vertex 'c'))
-- "cba"
--
-- >>> topologicalSort (Class.path "abd" <> Class.path "acd")
-- "dcba"
--
-- >>> topologicalSort (Class.path "aba")
-- "ab"
topologicalSort :: forall v . Ord v => Graph v -> [v]
topologicalSort = go . toAdjacencyMap . G.transpose . unGraph
  where go :: A.AdjacencyMap v -> [v]
        go graph
          = visitedOrder . fst
          . run
          . runState (Visited lowerBound [])
          . traverse_ visit
          . A.vertexList
          $ graph
          where visit :: v -> Eff '[State (Visited v)] ()
                visit v = do
                  isMarked <- Set.member v . visitedVertices <$> get
                  if isMarked then
                    pure ()
                  else do
                    modify' (extendVisited (Set.insert v))
                    traverse_ visit (Set.toList (A.postSet v graph))
                    modify' (extendOrder (v :))

data Visited v = Visited { visitedVertices :: !(Set v), visitedOrder :: [v] }

extendVisited :: (Set v -> Set v) -> Visited v -> Visited v
extendVisited f (Visited a b) = Visited (f a) b

extendOrder :: ([v] -> [v]) -> Visited v -> Visited v
extendOrder f (Visited a b) = Visited a (f b)


toAdjacencyMap :: Ord v => G.Graph v -> A.AdjacencyMap v
toAdjacencyMap = Class.toGraph


instance Lower (Graph vertex) where
  lowerBound = Class.empty

instance Semigroup (Graph vertex) where
  (<>) = overlay

instance Monoid (Graph vertex) where
  mempty = Class.empty
  mappend = (<>)

instance Ord vertex => Ord (Graph vertex) where
  compare (Graph G.Empty)           (Graph G.Empty)           = EQ
  compare (Graph G.Empty)           _                         = LT
  compare _                         (Graph G.Empty)           = GT
  compare (Graph (G.Vertex a))      (Graph (G.Vertex b))      = compare a b
  compare (Graph (G.Vertex _))      _                         = LT
  compare _                         (Graph (G.Vertex _))      = GT
  compare (Graph (G.Overlay a1 a2)) (Graph (G.Overlay b1 b2)) = (compare `on` Graph) a1 b1 <> (compare `on` Graph) a2 b2
  compare (Graph (G.Overlay _  _))  _                         = LT
  compare _                         (Graph (G.Overlay _ _))   = GT
  compare (Graph (G.Connect a1 a2)) (Graph (G.Connect b1 b2)) = (compare `on` Graph) a1 b1 <> (compare `on` Graph) a2 b2


class JSONVertex vertex where
  jsonVertexId :: vertex -> Text

instance (Ord vertex, ToJSON vertex, JSONVertex vertex) => ToJSON (Graph vertex) where
  toJSON     (Graph graph) = object ["vertices" .= G.vertexList graph,   "edges" .= (JSONEdge <$> G.edgeList graph)]
  toEncoding (Graph graph) = pairs  ("vertices" .= G.vertexList graph <> "edges" .= (JSONEdge <$> G.edgeList graph))

newtype JSONEdge vertex = JSONEdge (vertex, vertex)

instance (ToJSON vertex, JSONVertex vertex) => ToJSON (JSONEdge vertex) where
  toJSON     (JSONEdge (a, b)) = object ["source" .= jsonVertexId a,   "target" .= jsonVertexId b]
  toEncoding (JSONEdge (a, b)) = pairs  ("source" .= jsonVertexId a <> "target" .= jsonVertexId b)
