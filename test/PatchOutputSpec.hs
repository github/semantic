module PatchOutputSpec where

import Prologue
import Data.Functor.Both
import Info
import Range
import Renderer.Patch
import Source
import Syntax
import Category
import Test.Hspec

spec :: Spec
spec = parallel $
  describe "hunks" $
    it "empty diffs have empty hunks" $
        hunks (free . Free $ pure (Info (Range 0 0) StringLiteral 1) :< Leaf "") (both (SourceBlob (fromList "") "abcde" "path2.txt" (Just defaultPlainBlob)) (SourceBlob (fromList "") "xyz" "path2.txt" (Just defaultPlainBlob))) `shouldBe` [Hunk {offset = pure 0, changes = [], trailingContext = []}]
