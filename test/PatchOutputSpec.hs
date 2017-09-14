module PatchOutputSpec where

import Data.Blob
import Data.Functor.Both
import Data.Range
import Data.Record
import Diff
import Renderer.Patch
import Syntax
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "hunks" $ do
    it "empty diffs have empty hunks" $
        hunks (merge (Range 0 0 :. Nil, Range 0 0 :. Nil) (Leaf "")) (both (Blob mempty "abcde" "path2.txt" (Just defaultPlainBlob) Nothing) (Blob mempty "xyz" "path2.txt" (Just defaultPlainBlob) Nothing)) `shouldBe` [Hunk {offset = pure 0, changes = [], trailingContext = []}]
