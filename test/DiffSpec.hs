{-# LANGUAGE DataKinds #-}
module DiffSpec where

import Category
import Data.Functor.Both
import Data.Functor.Listable
import RWS
import Diff
import Info
import Interpreter
import Prologue
import SpecHelpers
import Term
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  let decorate = defaultFeatureVectorDecorator (category . headF)
  prop "equality is reflexive" $
    \ a -> let diff = unListableDiff a :: SyntaxDiff '[Category] in
      diff `shouldBe` diff

  prop "equal terms produce identity diffs" $
    \ a -> let term = decorate (unListableF a :: SyntaxTerm '[Category]) in
      diffCost (diffTerms (pure term)) `shouldBe` 0

  describe "beforeTerm" $ do
    prop "recovers the before term" $
      \ a b -> let diff = diffTerms (unListableF <$> both a b :: Both (SyntaxTerm '[Category])) in
        beforeTerm diff `shouldBe` Just (unListableF a)

  describe "afterTerm" $ do
    prop "recovers the after term" $
      \ a b -> let diff = diffTerms (unListableF <$> both a b :: Both (SyntaxTerm '[Category])) in
        afterTerm diff `shouldBe` Just (unListableF b)
