module PatchOutputSpec where

import Diff
import PatchOutput
import Range
import Source
import Syntax
import Control.Monad.Free
import Test.Hspec

spec :: Spec
spec = parallel $
  describe "hunks" $
    it "empty diffs have no hunks" $
      hunks (Free . Annotated (Info (Range 0 0) mempty, Info (Range 0 0) mempty) $ Leaf "") (SourceBlob (fromList "") "abcde" "path2.txt", SourceBlob (fromList "") "xyz" "path2.txt") `shouldBe` []
