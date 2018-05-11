{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances #-}
module Data.Graph
( Graph(..)
, Class.overlay
, Class.connect
, Class.vertex
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as Class

newtype Graph vertex = Graph (G.Graph vertex)
  deriving (Eq, Foldable, Functor, Class.Graph, Show, Class.ToGraph, Traversable)
