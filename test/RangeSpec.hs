module RangeSpec where

import Prologue
import Range
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "rangesAndWordsFrom" $ do
    it "should produce no ranges for the empty string" $
      rangesAndWordsFrom 0 mempty `shouldBe` []

    it "should produce no ranges for whitespace" $
      rangesAndWordsFrom 0 "  \t\n  " `shouldBe` []

    it "should produce a list containing the range of the string for a single-word string" $
      rangesAndWordsFrom 0 "word" `shouldBe` [ (Range 0 4, "word") ]

    it "should produce a list of ranges for whitespace-separated words" $
      rangesAndWordsFrom 0 "wordOne wordTwo" `shouldBe` [ (Range 0 7, "wordOne"), (Range 8 15, "wordTwo") ]

    it "should skip multiple whitespace characters" $
      rangesAndWordsFrom 0 "a  b" `shouldBe` [ (Range 0 1, "a"), (Range 3 4, "b") ]

    it "should skip whitespace at the start" $
      rangesAndWordsFrom 0 "  a b" `shouldBe` [ (Range 2 3, "a"), (Range 4 5, "b") ]

    it "should skip whitespace at the end" $
      rangesAndWordsFrom 0 "a b  " `shouldBe` [ (Range 0 1, "a"), (Range 2 3, "b") ]

    it "should produce ranges offset by its start index" $
      rangesAndWordsFrom 100 "a b" `shouldBe` [ (Range 100 101, "a"), (Range 102 103, "b") ]
