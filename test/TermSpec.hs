{-# LANGUAGE DataKinds #-}
module TermSpec where

import Category
import Data.String
import Data.Text.Arbitrary ()
import Data.Record
import Data.Record.Arbitrary ()
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
  describe "Term" $ do
    prop "equality is reflexive" $
      \ a -> toTerm a == toTerm (a :: ArbitraryTerm String ())

  describe "ArbitraryTerm" $ do
    prop "generates terms of a specific size" $ forAll ((arbitrary >>= \ n -> (,) n <$> termOfSize n) `suchThat` ((> 0) . fst)) $
      \ (n, term) -> arbitraryTermSize (term :: ArbitraryTerm String ()) `shouldBe` n
