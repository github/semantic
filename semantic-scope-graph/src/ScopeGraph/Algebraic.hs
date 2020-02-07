{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module ScopeGraph.Algebraic
  ( Graph (..)
  , Node (..)
  , fromPrimitive
  , edgeLabels
  ) where

import qualified Algebra.Graph.Labelled as Labelled
import           Algebra.Graph.ToGraph
import           Analysis.Name (Name)
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.ScopeGraph (EdgeLabel, Scope, ScopeGraph)
import qualified Data.ScopeGraph
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set

data Node addr = Node addr (Scope addr)
  deriving (Eq, Show, Ord)

newtype Graph addr = Graph { unGraph :: Labelled.Graph (Set EdgeLabel) (Node addr) }

instance Ord addr => ToGraph (Graph addr) where
  type ToVertex (Graph addr) = Node addr
  toGraph (Graph a) = toGraph a

edgeLabels :: Eq addr => Node addr -> Node addr -> Graph addr -> Set EdgeLabel
edgeLabels a b (Graph g) = Labelled.edgeLabel a b g

fromPrimitive :: ScopeGraph Name -> Graph Name
fromPrimitive (Data.ScopeGraph.ScopeGraph sg)
  = Graph (Labelled.edges (toList (Map.foldMapWithKey eachScope sg)))
    where
      eachScope :: Name -> Scope Name -> Seq (Set EdgeLabel, Node Name, Node Name)
      eachScope n parent = Map.foldMapWithKey eachEdge (Data.ScopeGraph.edges parent)
        where
          eachEdge :: EdgeLabel -> [Name] -> Seq (Set EdgeLabel, Node Name, Node Name)
          eachEdge lab connects = foldMap create scopes
            where
              inquire item = fmap (Node item) (Map.lookup item sg)
              scopes = catMaybes (fmap inquire connects)
              create neighbor = pure (Set.singleton lab, Node n parent, neighbor)
