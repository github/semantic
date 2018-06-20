{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances #-}
module Data.Graph
( Graph(..)
, Class.overlay
, Class.connect
, Class.vertex
, Lower(..)
, simplify
, topologicalSort
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as Class
import Data.Aeson
import Data.List (groupBy, sortBy)
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import qualified Data.Map.Monoidal as Monoidal
import Data.Ord (comparing)
import Prologue

-- | An algebraic graph with 'Ord', 'Semigroup', and 'Monoid' instances.
newtype Graph vertex = Graph (G.Graph vertex)
  deriving (Eq, Foldable, Functor, Class.Graph, Show, Class.ToGraph, Traversable)


simplify :: Ord vertex => Graph vertex -> Graph vertex
simplify (Graph graph) = Graph (G.simplify graph)


topologicalSort :: Ord v => Graph v -> [NonEmpty v]
topologicalSort
  = groupByInEdgeCount
  . Monoidal.pairs
  . edgeCountsByVertex

edgeCountsByVertex :: Ord v => Graph v -> Monoidal.Map v EdgeCounts
edgeCountsByVertex = Class.foldg
  lowerBound
  (flip Monoidal.singleton mempty)
  (<>)
  (\ outM inM
    -> outM
    <> inM
    <> foldMap (flip Monoidal.singleton (EdgeCounts 0 (length outM))) (Monoidal.keys inM)
    <> foldMap (flip Monoidal.singleton (EdgeCounts (length inM) 0))  (Monoidal.keys outM))

data EdgeCounts = EdgeCounts
  { inEdgeCount  :: {-# UNPACK #-} !Int
  , outEdgeCount :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup EdgeCounts where
  EdgeCounts in1 out1 <> EdgeCounts in2 out2 = EdgeCounts (in1 + in2) (out1 + out2)

instance Monoid EdgeCounts where
  mempty = EdgeCounts 0 0
  mappend = (<>)

groupByInEdgeCount :: Ord sum => [(v, sum)] -> [NonEmpty v]
groupByInEdgeCount = map (NonEmpty.fromList . map fst) . groupBy ((==) `on` snd) . sortBy (comparing snd)


instance Lower (Graph vertex) where
  lowerBound = Class.empty

instance Semigroup (Graph vertex) where
  (<>) = Class.overlay

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


instance (Ord vertex, ToJSON vertex) => ToJSON (Graph vertex) where
  toJSON     (Graph graph) = object ["vertices" .= G.vertexList graph,   "edges" .= (JSONEdge <$> G.edgeList graph)]
  toEncoding (Graph graph) = pairs  ("vertices" .= G.vertexList graph <> "edges" .= (JSONEdge <$> G.edgeList graph))

newtype JSONEdge vertex = JSONEdge (vertex, vertex)

instance ToJSON vertex => ToJSON (JSONEdge vertex) where
  toJSON     (JSONEdge (a, b)) = object ["source" .= a,   "target" .= b]
  toEncoding (JSONEdge (a, b)) = pairs  ("source" .= a <> "target" .= b)
