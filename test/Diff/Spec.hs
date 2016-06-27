{-# LANGUAGE DataKinds #-}
module Diff.Spec where

import Category
import Data.Record
import Data.Record.Arbitrary ()
import Data.String
import Diff
import Diff.Arbitrary
import Interpreter
import Prologue
import Term.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "mergeMaybe" $ do
    it "is symmetrical" $ pending

  describe "ArbitraryDiff" $ do
    prop "generates diffs of a specific size" . forAll ((arbitrary >>= \ n -> (,) n <$> diffOfSize n) `suchThat` ((> 0) . fst)) $
      \ (n, diff) -> arbitraryDiffSize (diff :: ArbitraryDiff String ()) `shouldBe` n

  describe "Diff" $ do
    prop "equality is reflexive" $
      \ a b -> let diff = diffTerms (free . Free) (==) diffCost (toTerm a) (toTerm (b :: ArbitraryTerm String (Record '[Category]))) in
        diff == diff

    prop "equal terms produce identity diffs" $
      \ a -> let term = toTerm (a :: ArbitraryTerm String (Record '[Category])) in
        diffCost (diffTerms (free . Free) (==) diffCost term term) == 0
