module PatchOutputSpec where

import Data.Functor.Both
import Diff
import Renderer.Patch
import Range
import Source
import Syntax
import Control.Monad.Free
import Test.Hspec

spec :: Spec
spec = parallel $
  describe "hunks" $
    it "empty diffs have empty hunks" $
        hunks (Free . Annotated (pure (Info (Range 0 0) mempty)) $ Leaf "") (Both (SourceBlob (fromList "") "abcde" "path2.txt" (Just defaultPlainBlob), SourceBlob (fromList "") "xyz" "path2.txt" (Just defaultPlainBlob))) `shouldBe` [Hunk {offset = Both (0, 0), changes = [], trailingContext = []}]
