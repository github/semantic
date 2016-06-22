module Data.Gram.Spec where

import Data.DList as DList hiding (toList)
import Data.Gram
import Data.Gram.Arbitrary ()
import Data.String
import Prologue
import Syntax
import Term
import Term.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)

spec :: Spec
spec = parallel $ do
  describe "pqGrams" $ do
    prop "produces grams with stems of the specified length" $ forAll (arbitrary `suchThat` (\ (_, p, q) -> p > 0 && q > 0)) $
      \ (term, p, q) -> pqGrams p q headF (toTerm term :: Term String String) `shouldSatisfy` all ((== p) . length . stem)

  describe "featureVector" $ do
    prop "produces a vector of the specified dimension" $ forAll (arbitrary `suchThat` ((> 0) . snd)) $
      \ (grams, d) -> length (featureVector (fromList (grams :: [Gram String])) d) `shouldBe` d
