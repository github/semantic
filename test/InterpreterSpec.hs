{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Category
import Data.RandomWalkSimilarity
import Data.Record
import Diff
import Info
import Interpreter
import Patch
import Prologue
import Syntax
import Term.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    let decorate = featureVectorDecorator (category . headF) 2 2 15
    let compare = ((==) `on` category . extract)
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = cofree $ (StringLiteral .: RNil) :< Leaf ("t\776" :: Text)
          termB = cofree $ (StringLiteral .: RNil) :< Leaf "\7831" in
          stripDiff (diffTerms wrap compare diffCost (decorate termA) (decorate termB)) `shouldBe` free (Pure (Replace termA termB))

    prop "produces correct diffs" $
      \ a b -> let diff = stripDiff $ diffTerms wrap compare diffCost (decorate (toTerm a)) (decorate (toTerm (b :: ArbitraryTerm Text (Record '[Category])))) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just (toTerm a), Just (toTerm b))

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let term = decorate (toTerm (a :: ArbitraryTerm Text (Record '[Category])))
                 diff = diffTerms wrap compare diffCost term term in
                 diffCost diff `shouldBe` 0

    let reverse' diff = case runFree diff of
          Free (h :< Indexed children) -> wrap (h :< Indexed (reverse children))
          Free c -> wrap c
          Pure a -> pure a
    let toTerm' c (ArbitraryTerm r f) = decorate (toTerm (ArbitraryTerm (setCategory r c) f))
    let root = cofree . ((pure 0 .: Program .: RNil) :<) . Indexed
    prop "produces unbiased deletions" $
      \ a b c -> let (a', b') = (toTerm' c a, toTerm' c (b :: ArbitraryTerm Text (Record '[Category]))) in
        stripDiff (diffTerms wrap compare diffCost (root [ a', b' ]) (root [ a' ])) `shouldBe` reverse' (stripDiff (diffTerms wrap compare diffCost (root [ b', a' ]) (root [ a' ])))

    prop "produces unbiased insertions" $
      \ a b c -> let (a', b') = (toTerm' c a, toTerm' c (b :: ArbitraryTerm Text (Record '[Category]))) in
        stripDiff (diffTerms wrap compare diffCost (root [ a' ]) (root [ a', b' ])) `shouldBe` reverse' (stripDiff (diffTerms wrap compare diffCost (root [ a' ]) (root [ b', a' ])))
