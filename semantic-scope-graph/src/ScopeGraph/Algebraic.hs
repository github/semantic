{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
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
import           Scope.Info

data Node addr
  = Node addr (Scope addr)
  | Informational (Info addr)
  deriving (Eq, Show, Ord)

newtype Graph addr = Graph { unGraph :: Labelled.Graph (Set EdgeLabel) (Node addr) }

instance Ord addr => ToGraph (Graph addr) where
  type ToVertex (Graph addr) = Node addr
  toGraph (Graph a) = toGraph a

edgeLabels :: Eq addr => Node addr -> Node addr -> Graph addr -> Set EdgeLabel
edgeLabels a b (Graph g) = Labelled.edgeLabel a b g

fromPrimitive :: ScopeGraph Name -> Graph Name
fromPrimitive omnigraph@(Data.ScopeGraph.ScopeGraph sg)
  = Graph (Labelled.edges (toList (Map.foldMapWithKey eachScope sg)))
    where
      eachScope :: Name -> Scope Name -> Seq (Set EdgeLabel, Node Name, Node Name)
      eachScope n parentScope
        = Map.foldMapWithKey eachEdge (Data.ScopeGraph.edges parentScope)
        <> foldMap eachInfo (Data.ScopeGraph.declarations parentScope)
        where
          parent = Node n parentScope
          eachInfo :: Info Name -> Seq (Set EdgeLabel, Node Name, Node Name)
          eachInfo i =
            let
              standard = (Set.singleton Data.ScopeGraph.Within, parent, Informational i)
              assoc = infoAssociatedScope i
            in
              (case (assoc, assoc >>= flip Data.ScopeGraph.lookupScope omnigraph) of
                (Just a, Just sc) ->
                  [ standard
                  , (Set.singleton Data.ScopeGraph.Within, Informational i, Node a sc)
                  ]
                _ -> [standard])
          eachEdge :: EdgeLabel -> [Name] -> Seq (Set EdgeLabel, Node Name, Node Name)
          eachEdge lab connects = foldMap create scopes
            where
              inquire item = fmap (Node item) (Map.lookup item sg)
              scopes = catMaybes (fmap inquire connects)
              create neighbor = pure (Set.singleton lab, parent, neighbor)
