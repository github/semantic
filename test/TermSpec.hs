module TermSpec where

import ArbitraryTerm
import Category
import Data.String
import Data.Functor.Foldable
import Data.Text.Arbitrary ()
import Diff
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

  describe "ArbitraryTerm" $
    prop "generates terms of a specific size" $ forAll ((arbitrary >>= \ n -> (,) n <$> termOfSize n) `suchThat` ((> 0) . fst)) $
      \ (n, term) -> cata (succ . sum) (toTerm (term :: ArbitraryTerm String ())) `shouldBe` n

  describe "Diff" $ do
    prop "equality is reflexive" $
      \ a b -> let diff = interpret comparable diffCost (toTerm a) (toTerm (b :: ArbitraryTerm String CategorySet)) in
        diff == diff

    prop "equal terms produce identity diffs" $
      \ a -> let term = toTerm (a :: ArbitraryTerm String CategorySet) in
        diffCost (interpret comparable diffCost term term) == 0
