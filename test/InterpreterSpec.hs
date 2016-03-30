module InterpreterSpec where

import qualified Interpreter as I
import Range
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free
import Patch
import Info
import Category
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      I.interpret comparable (Info range mempty :< Leaf "t\776") (Info range2 mempty :< Leaf "\7831") `shouldBe`
      Pure (Replace (Info range mempty :< Leaf "t\776") (Info range2 mempty :< Leaf "\7831"))

    where
      range = Range 0 2
      range2 = Range 0 1
