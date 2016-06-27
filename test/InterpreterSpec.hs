module InterpreterSpec where

import Prologue
import Diff
import Data.Record
import qualified Interpreter as I
import Range
import Syntax
import Patch
import Info
import Category
import Test.Hspec

spec :: Spec
spec = parallel $
  describe "interpret" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let infoA = Range 0 2 .: StringLiteral .: 0 .: 0 .: RNil
          infoB = Range 0 1 .: StringLiteral .: 0 .: 0 .: RNil in
          I.diffTerms (free . Free) ((==) `on` extract) diffCost (cofree (infoA :< Leaf "t\776")) (cofree (infoB :< Leaf "\7831")) `shouldBe`
          free (Pure (Replace (cofree (infoA :< Leaf "t\776")) (cofree (infoB :< Leaf "\7831"))))
