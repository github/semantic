{-# LANGUAGE PackageImports #-}

module Data.Graph.Spec (spec) where

import SpecHelpers

import "semantic" Data.Graph
import qualified Algebra.Graph.Class as Class

spec :: Spec
spec = describe "Data.Graph" $
  it "has a valid topological sort" $ do
    let topo = topologicalSort
    topo (Class.path "ab") `shouldBe` "ba"
    topo (Class.path "abc") `shouldBe` "cba"
    topo ((vertex 'a' `connect` vertex 'b') `connect` vertex 'c') `shouldBe` "cba"
    topo (vertex 'a' `connect` (vertex 'b' `connect` vertex 'c')) `shouldBe` "cba"
    topo ((vertex 'a' `connect` vertex 'b') <> (vertex 'a' `connect` vertex 'c')) `shouldBe` "cba"
    topo (Class.path "abd" <> Class.path "acd") `shouldBe` "dcba"
    topo (Class.path "aba") `shouldBe` "ab"
