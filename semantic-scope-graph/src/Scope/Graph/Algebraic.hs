{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
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
import           Analysis.Name (Name, nameI)
import           Data.Foldable
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Scope.Graph.AdjacencyList (ScopeGraph (..), lookupScope)
import qualified Scope.Graph.AdjacencyList as AL
import           Scope.Info
import           Scope.Path
import           Scope.Reference
import           Scope.Scope (Scope)
import qualified Scope.Scope as Scope
import           Scope.Types

data Node addr
  = Node addr (Scope addr)
  | Informational (Info addr)
  | Declares (Info addr)
  deriving (Eq, Show, Ord)


data Link
  = Declaring
  | Referencing (Set EdgeLabel)
    deriving (Eq, Ord, Show)

instance Semigroup Link where
  Declaring <> Declaring = Declaring
  Declaring <> Referencing a = Referencing a
  Referencing a <> Declaring = Referencing a
  Referencing a <> Referencing b = Referencing (a <> b)

instance Monoid Link where
  mempty = Referencing mempty

referencing :: EdgeLabel -> Link
referencing = Referencing . Set.singleton

newtype Graph addr = Graph { unGraph :: Labelled.Graph Link (Node addr) }
  deriving (Eq, Show)

instance Ord addr => ToGraph (Graph addr) where
  type ToVertex (Graph addr) = Node addr
  toGraph (Graph a) = toGraph a

edgeLabel :: Eq addr => Node addr -> Node addr -> Graph addr -> Link
edgeLabel a b (Graph g) = Labelled.edgeLabel a b g


unfurl :: ScopeGraph Name -> Name -> Scope Name -> Seq (Link, Node Name, Node Name)
unfurl sg addr parentScope = Scope.fold eachEdge eachReference eachInfo parentScope
  where
    parent :: Node Name
    parent = Node addr parentScope

    eachReference :: Reference -> [ReferenceInfo] -> Path Name -> (Seq (Link, Node Name, Node Name))
    eachReference (Reference r) _i p = case p of
      Hole             -> mempty
      DPath decl _pos  -> case AL.declarationByName r decl sg of
        Just neighb -> [(Declaring, parent, Informational neighb)]
        Nothing     -> mempty
      EPath lab s _next -> eachEdge lab [s]

    eachInfo :: Info Name -> Seq (Link, Node Name, Node Name)
    eachInfo i =
      let
        standard = (Declaring, parent, Informational i)
        assoc = infoAssociatedScope i
      in
        (case (assoc, assoc >>= flip AL.lookupScope sg) of
          (Just a, Just sc) ->
            [ (Declaring, Declares i, Node a sc)
            ]
          _ -> [standard])
    eachEdge :: EdgeLabel -> [Name] -> Seq (Link, Node Name, Node Name)
    eachEdge lab connects = foldMap create scopes
      where
        inquire item = fmap (Node item) (AL.lookupScope item sg)
        scopes = catMaybes (fmap inquire connects)
        create neighbor = pure (referencing lab, parent, neighbor)


fromPrimitive :: ScopeGraph Name -> Graph Name
fromPrimitive sg
  = Graph . Labelled.edges . toList . AL.foldGraph eachAddr (nameI 0) $ sg
    where
      eachAddr addr fn
        = foldMap @[] fn [Lexical .. Superclass]
        <> maybe mempty (unfurl sg addr) (lookupScope addr sg)

