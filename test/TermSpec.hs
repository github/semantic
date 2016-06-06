module TermSpec where

import ArbitraryTerm
import Data.String
import Data.Functor.Foldable
import Data.Text.Arbitrary ()
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

  describe "ArbitraryTerm" $
    prop "generates terms of a specific size" $ forAll ((arbitrary >>= \ n -> (,) n <$> termOfSize n) `suchThat` ((> 0) . fst)) $
      \ (n, term) -> cata (succ . sum) (toTerm (term :: ArbitraryTerm String ())) `shouldBe` n

  describe "ArbitraryDiff" $
    prop "generates diffs of a specific size" $ forAll ((arbitrary >>= \ n -> (,) n <$> diffOfSize n) `suchThat` ((> 0) . fst)) $
      \ (n, diff) -> cata (succ . sum) (fmap (cata (succ . sum)) <$> (toDiff (diff :: ArbitraryDiff String ()))) `shouldBe` n

  describe "Diff" $ do
    prop "equality is reflexive" $
      \ a b -> let diff = diffTerms (free . Free) (==) diffCost (toTerm a) (toTerm (b :: ArbitraryTerm String CategorySet)) in
        diff == diff

    prop "equal terms produce identity diffs" $
      \ a -> let term = toTerm (a :: ArbitraryTerm String CategorySet) in
        diffCost (diffTerms (free . Free) (==) diffCost term term) == 0
