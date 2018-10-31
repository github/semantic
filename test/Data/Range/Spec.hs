{-# LANGUAGE ScopedTypeVariables #-}

module Data.Range.Spec where

import Data.Range
import SpecHelpers

spec :: Spec
spec = describe "Data.Range" $
  prop "should have an associative Semigroup instance" $
    \(a, b, c) -> a <> (b <> c) `shouldBe` (a <> b) <> (c :: Range)
