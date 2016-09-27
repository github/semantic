module TermSpec where

import Data.Text.Arbitrary ()
import Prologue
import Term.Arbitrary
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "Term" $ do
    prop "equality is reflexive" $
      \ a -> toTerm a `shouldBe` toTerm (a :: ArbitraryTerm Text ())

  describe "ArbitraryTerm" $ do
    prop "generates terms of a specific size" . forAll ((arbitrary >>= \ n -> (,) n <$> termOfSize n) `suchThat` ((> 0) . fst)) $
      \ (n, term) -> arbitraryTermSize (term :: ArbitraryTerm Text ()) `shouldBe` n
