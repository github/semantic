module PatchOutputSpec where

import Prologue
import Data.Functor.Both
import Data.String
import Diff
import Info
import Range
import Renderer.Patch
import Source
import Syntax
import Test.Hspec

spec :: Spec
spec = parallel $
  describe "hunks" $
    it "empty diffs have empty hunks" $
        hunks (free . Free $ pure (Info (Range 0 0) mempty 1) :< Leaf "") (both (SourceBlob (fromList "") "abcde" "path2.txt" (Just defaultPlainBlob)) (SourceBlob (fromList "") "xyz" "path2.txt" (Just defaultPlainBlob))) `shouldBe` [Hunk {offset = pure 0, changes = [], trailingContext = []}]
