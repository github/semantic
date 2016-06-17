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
  describe "interpret" $
    it "returns a replacement when comparing two unicode equivalent terms" $
      I.diffTerms (free . Free) ((==) `on` extract) diffCost (cofree ((range .: StringLiteral .: 0 .: 0 .: RNil) :< Leaf "t\776")) (cofree ((range2 .: StringLiteral .: 0 .: 0 .: RNil) :< Leaf "\7831")) `shouldBe`
      free (Pure (Replace (cofree ((range .: StringLiteral .: 0 .: 0 .: RNil) :< Leaf "t\776")) (cofree ((range2 .: StringLiteral .: 0 .: 0 .: RNil) :< Leaf "\7831"))))

    where
      range = Range 0 2
      range2 = Range 0 1
