{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Scope.Graph.Algebraic
  ( Graph (..)
  , Node (..)
  , Link (..)
  , fromPrimitive
  , edgeLabel
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


data Link
  = Strong
  | Parent (Set EdgeLabel)
    deriving (Eq, Ord, Show)

instance Semigroup Link where
  Strong <> Strong = Strong
  Strong <> Parent a = Parent a
  Parent a <> Strong = Parent a
  Parent a <> Parent b = Parent (a <> b)

instance Monoid Link where
  mempty = Parent mempty

parenting :: EdgeLabel -> Link
parenting = Parent . Set.singleton

newtype Graph addr = Graph { unGraph :: Labelled.Graph Link (Node addr) }

instance Ord addr => ToGraph (Graph addr) where
  type ToVertex (Graph addr) = Node addr
  toGraph (Graph a) = toGraph a

edgeLabel :: Eq addr => Node addr -> Node addr -> Graph addr -> Link
edgeLabel a b (Graph g) = Labelled.edgeLabel a b g

fromPrimitive :: ScopeGraph Name -> Graph Name
fromPrimitive omnigraph@(Data.ScopeGraph.ScopeGraph sg)
  = Graph (Labelled.edges (toList (Map.foldMapWithKey eachScope sg)))
    where
      eachScope :: Name -> Scope Name -> Seq (Link, Node Name, Node Name)
      eachScope n parentScope
        = Map.foldMapWithKey eachEdge (Data.ScopeGraph.edges parentScope)
        <> foldMap eachInfo (Data.ScopeGraph.declarations parentScope)
        where
          parent = Node n parentScope
          eachInfo :: Info Name -> Seq (Link, Node Name, Node Name)
          eachInfo i =
            let
              standard = (Strong, parent, Informational i)
              assoc = infoAssociatedScope i
            in
              (case (assoc, assoc >>= flip Data.ScopeGraph.lookupScope omnigraph) of
                (Just a, Just sc) ->
                  [ (Strong, Informational i, Node a sc)
                  ]
                _ -> [standard])
          eachEdge :: EdgeLabel -> [Name] -> Seq (Link, Node Name, Node Name)
          eachEdge lab connects = foldMap create scopes
            where
              inquire item = fmap (Node item) (Map.lookup item sg)
              scopes = catMaybes (fmap inquire connects)
              create neighbor = pure (parenting lab, parent, neighbor)
