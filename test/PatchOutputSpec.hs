module PatchOutputSpec where

import Control.Monad.Free
import Data.Functor.Both
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
        hunks (Free . Annotated (pure (Info (Range 0 0) mempty 1)) $ Leaf "") (Both (SourceBlob (fromList "") "abcde" "path2.txt" (Just defaultPlainBlob), SourceBlob (fromList "") "xyz" "path2.txt" (Just defaultPlainBlob))) `shouldBe` [Hunk {offset = Both (0, 0), changes = [], trailingContext = []}]
