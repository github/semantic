{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Category
import Diff
import Data.Record
import qualified Data.Vector as Vector
import Interpreter
import Patch
import Prologue
import Syntax
import Term.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = cofree $ (StringLiteral .: Vector.singleton (0 :: Double) .: RNil) :< Leaf ("t\776" :: Text)
          termB = cofree $ (StringLiteral .: Vector.singleton (0 :: Double) .: RNil) :< Leaf "\7831" in
          diffTerms wrap ((==) `on` extract) diffCost termA termB `shouldBe` free (Pure (Replace termA termB))

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms wrap ((==) `on` extract) diffCost (toTerm a) (toTerm b) :: Diff Text (Record '[Category, Vector.Vector Double]) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just (toTerm a), Just (toTerm b))

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let term = toTerm a
                 diff = diffTerms wrap ((==) `on` extract) diffCost term term :: Diff Text (Record '[Category, Vector.Vector Double]) in
                 diffCost diff `shouldBe` 0


instance Arbitrary a => Arbitrary (Vector.Vector a) where
  arbitrary = Vector.fromList <$> listOf1 arbitrary
  shrink a = Vector.fromList <$> shrink (Vector.toList a)
