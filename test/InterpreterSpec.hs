{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Category
import Data.Functor.Foldable
import Data.RandomWalkSimilarity
import Data.Record
import Diff
import Diffing
import Info
import Interpreter
import Patch
import Prologue
import Syntax
import Term.Arbitrary
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    let decorate = defaultFeatureVectorDecorator (category . headF)
    let compare = ((==) `on` category . extract)
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = cofree $ (StringLiteral .: RNil) :< Leaf ("t\776" :: Text)
          termB = cofree $ (StringLiteral .: RNil) :< Leaf "\7831" in
          stripDiff (diffTerms wrap compare diffCost getLabel (decorate termA) (decorate termB)) `shouldBe` replacing termA termB

    prop "produces correct diffs" $
      \ a b -> let diff = stripDiff $ diffTerms wrap compare diffCost getLabel (decorate (toTerm a)) (decorate (toTerm (b :: ArbitraryTerm Text (Record '[Category])))) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just (toTerm a), Just (toTerm b))

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let term = decorate (toTerm (a :: ArbitraryTerm Text (Record '[Category])))
                 diff = diffTerms wrap compare diffCost getLabel term term in
                 diffCost diff `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = decorate (cofree ((StringLiteral .: RNil) :< Indexed [ cofree ((StringLiteral .: RNil) :< Leaf s) ]))
          root = cofree . ((pure 0 .: Program .: RNil) :<) . Indexed in
      stripDiff (diffTerms wrap compare diffCost getLabel (root [ term "b" ]) (root [ term "a", term "b" ])) `shouldBe` wrap (pure (Program .: RNil) :< Indexed [ inserting (stripTerm (term "a")), cata wrap (fmap pure (stripTerm (term "b"))) ])
