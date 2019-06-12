{-# LANGUAGE PackageImports #-}

module Data.Graph.Spec where

import SpecHelpers

import "semantic" Data.Graph
import qualified Algebra.Graph.Class as Class
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec = testGroup "Data.Graph" . pure .
  testCase "has an in-visited-order topological sort" $ do
    let topo = topologicalSort
    topo (Class.path "ab") @?= "ba"
    topo (Class.path "abc") @?= "cba"
    topo ((vertex 'a' `connect` vertex 'b') `connect` vertex 'c') @?= "cba"
    topo (vertex 'a' `connect` (vertex 'b' `connect` vertex 'c')) @?= "cba"
    topo ((vertex 'a' `connect` vertex 'b') <> (vertex 'a' `connect` vertex 'c')) @?= "cba"
    topo (Class.path "abd" <> Class.path "acd") @?= "dcba"
    topo (Class.path "aba") @?= "ab"
