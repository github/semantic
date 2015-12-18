module PatchOutputSpec where

import Diff
import PatchOutput
import Range
import Syntax
import Control.Monad.Free
import Test.Hspec

spec :: Spec
spec = do
  describe "hunks" $ do
    it "empty diffs have no hunks" $
      hunks (Free . Annotated (Info (Range 0 0) mempty, Info (Range 0 0) mempty) $ Leaf "") "" "" `shouldBe` []
