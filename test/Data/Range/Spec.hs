module Data.Range.Spec where

import Data.Functor.Listable
import Data.Range
import Test.Tasty
import Test.Tasty.LeanCheck

spec :: TestTree
spec =
  testProperty "Data.Range: should have an associative Semigroup instance" $
    \(a, b, c) -> a <> (b <> c) == (a <> b) <> (c :: Range)
