module Source.Spec where

import Prologue
import Range
import Source
import SourceSpan
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "actualLineRanges" $ do
    prop "produces 1 more range than there are newlines" $
      \ s -> length (actualLineRanges (totalRange s) (fromList s)) `shouldBe` succ (length (filter (== '\n') s))

    prop "produces exhaustive ranges" $
      \ s -> let source = fromList s in
        foldMap (`slice` source) (actualLineRanges (totalRange s) source) `shouldBe` source

  describe "sourceSpanToRange" $ do
    it "_" $
      let source = fromList "a\nb" in
      sourceSpanToRange source (SourceSpan "" (SourcePos 0 0) (SourcePos 1 1)) `shouldBe` totalRange source
