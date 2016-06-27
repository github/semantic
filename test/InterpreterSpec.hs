module InterpreterSpec where

import Prologue
import Diff
import Data.Record
import Interpreter
import Syntax
import Patch
import Category
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = cofree $ (StringLiteral .: RNil) :< Leaf "t\776"
          termB = cofree $ (StringLiteral .: RNil) :< Leaf "\7831" in
          diffTerms (free . Free) ((==) `on` extract) diffCost termA termB `shouldBe` free (Pure (Replace termA termB))
