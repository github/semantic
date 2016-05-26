module InterpreterSpec where

import Prologue
import Diff
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
      I.interpret comparable diffCost (cofree (Info range mempty 0 :< Leaf "t\776")) (cofree (Info range2 mempty 0 :< Leaf "\7831")) `shouldBe`
      free (Pure (Replace (cofree (Info range mempty 0 :< Leaf "t\776")) (cofree (Info range2 mempty 0 :< Leaf "\7831"))))

    where
      range = Range 0 2
      range2 = Range 0 1
