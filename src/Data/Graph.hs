{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances #-}
module Data.Graph
( Graph(..)
, Class.overlay
, Class.connect
, Class.vertex
, Lower(..)
, simplify
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as Class
import Data.Aeson
import Prologue

-- | An algebraic graph with 'Ord', 'Semigroup', and 'Monoid' instances.
newtype Graph vertex = Graph (G.Graph vertex)
  deriving (Eq, Foldable, Functor, Class.Graph, Show, Class.ToGraph, Traversable)


simplify :: Ord vertex => Graph vertex -> Graph vertex
simplify (Graph graph) = Graph (G.simplify graph)


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
