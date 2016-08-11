{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Category
import Data.Functor.Foldable
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
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = decorate . cofree $ (StringLiteral .: RNil) :< Leaf ("t\776" :: Text)
          termB = decorate . cofree $ (StringLiteral .: RNil) :< Leaf "\7831" in
          diffTerms wrap ((==) `on` extract) diffCost termA termB `shouldBe` free (Pure (Replace termA termB))

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms wrap ((==) `on` extract) diffCost (decorate (toTerm a)) (decorate (toTerm (b :: ArbitraryTerm Text (Record '[Category])))) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just (decorate (toTerm a)), Just (decorate (toTerm b)))

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let term = decorate (toTerm (a :: ArbitraryTerm Text (Record '[Category])))
                 diff = diffTerms wrap ((==) `on` extract) diffCost term term in
                 diffCost diff `shouldBe` 0

    prop "produces unbiased deletions" $
      \ a b -> let (a', b') = (decorate (toTerm a), decorate (toTerm (b :: ArbitraryTerm Text (Record '[Category]))))
                   termA = cofree $ (pure 0 .: Program .: RNil) :< Indexed [ a', b' ]
                   termB = cofree $ (pure 0 .: Program .: RNil) :< Indexed [ a' ] in
        diffTerms wrap ((==) `on` extract) diffCost termA termB `shouldBe` free (Free (pure (pure 0 .: Program .: RNil) :< Indexed [ cata wrap (fmap pure a'), deleting b' ]))
