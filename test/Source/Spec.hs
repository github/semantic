module Source.Spec where

import Prologue
import Range
import Source
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "actualLineRanges" $ do
    prop "produces 1 more range than there are newlines" $
      \ s -> length (actualLineRanges (totalRange s) (fromList s)) `shouldBe` succ (length (filter (== '\n') s))
