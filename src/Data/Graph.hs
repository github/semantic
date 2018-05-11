module Data.Graph
( Graph(..)
) where

import qualified Algebra.Graph as G

newtype Graph vertex = Graph (G.Graph vertex)
