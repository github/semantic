module Data.Gram.Spec where

import Control.Arrow ((&&&))
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
  describe "pqGrams" $
    let getChildren (_ :< f) = case f of
          Leaf _ -> []
          Indexed c -> c
          Fixed c -> c
          Keyed c -> toList c in
    prop "produces grams with stems of the specified length" $ forAll (arbitrary `suchThat` ((> 0) . fst . snd)) $
      \ (term, (p, q)) -> pqGrams p q (headF &&& getChildren) (toTerm term :: Term String String) `shouldSatisfy` all ((<= p) . length . stem) 

  describe "featureVector" $ do
    prop "produces a vector of the specified dimension" $ forAll (arbitrary `suchThat` ((> 0) . snd)) $
      \ (grams, d) -> length (featureVector (fromList (grams :: [Gram String])) d) `shouldBe` d
