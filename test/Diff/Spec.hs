{-# LANGUAGE DataKinds #-}
module Diff.Spec where

import Category
import Data.Bifunctor.Join
import Data.Functor.Listable
import Data.String
import Diff
import Diffing (getLabel)
import Interpreter
import Patch
import Patch.Arbitrary ()
import Prologue
import Term
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  prop "equality is reflexive" $
    \ a -> let diff = unListableDiff a :: SyntaxDiff String '[Category] in
      diff `shouldBe` diff

  prop "equal terms produce identity diffs" $
    \ a -> let term = unListableF a :: SyntaxTerm String '[Category] in
      diffCost (diffTerms wrap (==) diffCost getLabel term term) `shouldBe` 0

  describe "beforeTerm" $ do
    prop "recovers the before term" $
      \ a b -> let diff = diffTerms wrap (==) diffCost getLabel (unListableF a) (unListableF b :: SyntaxTerm String '[Category]) in
        beforeTerm diff `shouldBe` Just (unListableF a)

  describe "afterTerm" $ do
    prop "recovers the after term" $
      \ a b -> let diff = diffTerms wrap (==) diffCost getLabel (unListableF a) (unListableF b :: SyntaxTerm String '[Category]) in
        afterTerm diff `shouldBe` Just (unListableF b)

unListableDiff :: Functor f => ListableF (Free (TermF f (ListableF (Join (,)) annotation))) (Patch (ListableF (Term f) annotation)) -> Diff f annotation
unListableDiff diff = transFreeT (first unListableF) $ fmap unListableF <$> unListableF diff
