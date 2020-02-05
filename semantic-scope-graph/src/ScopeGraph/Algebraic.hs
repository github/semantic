module ScopeGraph.Algebraic where

import qualified Algebra.Graph.Labelled as Labelled
import           Analysis.Name (Name)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.ScopeGraph (EdgeLabel, Path, Scope, ScopeGraph)
import qualified Data.ScopeGraph

algebrize :: ScopeGraph Name -> Labelled.Graph [EdgeLabel] (Scope Name)
algebrize (Data.ScopeGraph.ScopeGraph sg) = Labelled.edges (appEndo (Map.foldMapWithKey eachScope sg) mempty)
  where
     eachScope :: Name -> Scope Name -> Endo [([EdgeLabel], Scope Name, Scope Name)]
     eachScope _n scope = Map.foldMapWithKey eachEdge (Data.ScopeGraph.edges scope)
       where
         eachEdge :: EdgeLabel -> [Name] -> Endo [([EdgeLabel], Scope Name, Scope Name)]
         eachEdge lab connects =
           let
             scopes = catMaybes (fmap (`Map.lookup` sg) connects)
             eachScope s = Endo ([([lab], scope, s)] <>)
           in foldMap eachScope scopes

allPaths :: ScopeGraph address -> [Path address]
allPaths = (`appEndo` []) . Map.foldMapWithKey go . Data.ScopeGraph.unScopeGraph
  where
    go :: address -> Scope address -> Endo [Path address]
    go addr = Map.foldMapWithKey go2 . Data.ScopeGraph.references
      where
        go2 :: Data.ScopeGraph.Reference
            -> ([Data.ScopeGraph.ReferenceInfo], Path address1)
            -> Endo [Path address1]
        go2 _ (_, a) = Endo (a :)
