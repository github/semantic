module PatchOutputSpec where

import Prologue
import Data.Functor.Both
import Data.Record
import Range
import Renderer.Patch
import Source
import Syntax
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "hunks" $ do
    it "empty diffs have empty hunks" $
        hunks (wrap $ pure (Range 0 0 :. Nil) :< Leaf ("" :: Text)) (both (SourceBlob Source.empty "abcde" "path2.txt" (Just defaultPlainBlob) Nothing) (SourceBlob Source.empty "xyz" "path2.txt" (Just defaultPlainBlob) Nothing)) `shouldBe` [Hunk {offset = pure 0, changes = [], trailingContext = []}]
