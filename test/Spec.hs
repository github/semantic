module Main where

import Split
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "split" $ do
    it "two single line elements should concatenate into a single line" $
      adjoinLines ([], []) ([], []) `shouldBe` ([], [])
