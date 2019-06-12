{-# LANGUAGE DataKinds #-}
module Data.Term.Spec (spec) where

import Data.Functor.Listable
import Data.Term
import Test.Tasty
import Test.Tasty.LeanCheck

spec :: TestTree
spec = testGroup "Data.Term" . pure .
  testProperty "equality is reflexive" $
    \ a -> a == (a :: Term ListableSyntax ())
