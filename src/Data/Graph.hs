{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Data.Graph
( Graph(..)
, overlay
, connect
, vertex
, Lower(..)
, simplify
, topologicalSort
, VertexTag(..)
, Edge(..)
, vertexList
, edgeList
) where

import Prologue

import qualified Algebra.Graph as G
import qualified Algebra.Graph.AdjacencyMap as A
import           Algebra.Graph.Class (connect, overlay, vertex)
import qualified Algebra.Graph.Class as Class
import qualified Algebra.Graph.ToGraph as Class
import           Control.Effect.State
import           Control.Lens (view)
import           Data.Aeson
import qualified Data.Set as Set
import           Proto.Semantic as P
import           Proto.Semantic_Fields as P

-- | An algebraic graph with 'Ord', 'Semigroup', and 'Monoid' instances.
newtype Graph vertex = Graph { unGraph :: G.Graph vertex }
  deriving (Alternative, Applicative, Eq, Functor, Monad, Show, Class.Graph)

instance Ord t => Class.ToGraph (Graph t) where
  type ToVertex (Graph t) = t
  toGraph = Class.toGraph . unGraph

simplify :: Ord vertex => Graph vertex -> Graph vertex
simplify (Graph graph) = Graph (G.simplify graph)


-- | Sort a graph’s vertices topologically. Specced in @Data.Graph.Spec@.
topologicalSort :: forall v . Ord v => Graph v -> [v]
topologicalSort = go . Class.toAdjacencyMap . G.transpose . unGraph
  where go :: A.AdjacencyMap v -> [v]
        go graph
          = visitedOrder . fst
          . run
          . runState (Visited lowerBound [])
          . traverse_ visit
          . A.vertexList
          $ graph
          where visit :: (Member (State (Visited v)) sig, Carrier sig m) => v -> m ()
                visit v = do
                  isMarked <- Set.member v . visitedVertices <$> get
                  if isMarked then
                    pure ()
                  else do
                    modify (extendVisited (Set.insert v))
                    traverse_ visit (Set.toList (A.postSet v graph))
                    modify (extendOrder (v :))

data Visited v = Visited { visitedVertices :: !(Set v), visitedOrder :: [v] }

extendVisited :: (Set v -> Set v) -> Visited v -> Visited v
extendVisited f (Visited a b) = Visited (f a) b

extendOrder :: ([v] -> [v]) -> Visited v -> Visited v
extendOrder f (Visited a b) = Visited a (f b)

vertexList :: Ord v => Graph v -> [v]
vertexList = G.vertexList . unGraph

edgeList :: Ord v => Graph v -> [Edge v]
edgeList = fmap Edge . G.edgeList . unGraph

-- Instances

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


class VertexTag vertex where
  uniqueTag :: vertex -> Int

instance VertexTag DiffTreeVertex where uniqueTag = fromIntegral . view diffVertexId
instance VertexTag TermVertex where uniqueTag = fromIntegral . view vertexId

instance (Ord vertex, ToJSON vertex, VertexTag vertex) => ToJSON (Graph vertex) where
  toJSON     (Graph graph) = object ["vertices" .= G.vertexList graph,   "edges" .= (Edge <$> G.edgeList graph)]
  toEncoding (Graph graph) = pairs  ("vertices" .= G.vertexList graph <> "edges" .= (Edge <$> G.edgeList graph))

newtype Edge vertex = Edge (vertex, vertex)

instance (ToJSON vertex, VertexTag vertex) => ToJSON (Edge vertex) where
  toJSON     (Edge (a, b)) = object ["source" .= show (uniqueTag a),   "target" .= show (uniqueTag b)]
  toEncoding (Edge (a, b)) = pairs  ("source" .= show (uniqueTag a) <>  "target" .= show (uniqueTag b))
