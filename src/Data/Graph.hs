{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances #-}
module Data.Graph
( Graph(..)
, Class.overlay
, Class.connect
, Class.vertex
, simplify
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as Class
import Prologue

newtype Graph vertex = Graph (G.Graph vertex)
  deriving (Eq, Foldable, Functor, Class.Graph, Show, Class.ToGraph, Traversable)


simplify :: Ord v => Graph v -> Graph v
simplify (Graph graph) = Graph (G.simplify graph)


instance Semigroup (Graph v) where
  (<>) = Class.overlay
