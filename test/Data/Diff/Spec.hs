{-# LANGUAGE DataKinds #-}
module Data.Diff.Spec (spec) where

import Data.Diff
import Data.Functor.Listable (ListableSyntax)
import Test.Tasty
import Test.Tasty.LeanCheck

spec :: TestTree
spec = testGroup "Data.Term" . pure .
  testProperty "equality is reflexive" $
    \ a -> a == (a :: Diff ListableSyntax () ())
