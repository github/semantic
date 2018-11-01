{-# LANGUAGE DataKinds #-}
module Data.Diff.Spec where

import Data.Diff
import Data.Functor.Listable (ListableSyntax)
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  prop "equality is reflexive" $
    \ diff -> diff `shouldBe` (diff :: Diff ListableSyntax () ())
