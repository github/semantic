module TermSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Text.Arbitrary ()

import Prologue
import Data.String
import Category
import Interpreter
import Diff
import ArbitraryTerm

spec :: Spec
spec = parallel $ do
  describe "Term" $ do
    prop "equality is reflexive" $
      \ a -> toTerm a == toTerm (a :: ArbitraryTerm String ())

  describe "Diff" $ do
    prop "equality is reflexive" $
      \ a b -> let diff = interpret comparable diffCost (toTerm a) (toTerm (b :: ArbitraryTerm String CategorySet)) in
        diff == diff

    prop "equal terms produce identity diffs" $
      \ a -> let term = toTerm (a :: ArbitraryTerm String CategorySet) in
        diffCost (interpret comparable diffCost term term) == 0
