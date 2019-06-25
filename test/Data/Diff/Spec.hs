{-# LANGUAGE DataKinds #-}
module Data.Diff.Spec (spec) where

import Data.Diff
import Data.Functor.Listable (ListableSyntax)
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  prop "equality is reflexive" $
    \ diff -> diff `shouldBe` (diff :: Diff ListableSyntax () ())
