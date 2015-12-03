module Main where

import Split
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "split" $ do
    it "empty lines are the left unit" $
      adjoinLines ([], []) ([ Line "a" ], [ Line "b" ]) `shouldBe` ([ Line "a" ], [ Line "b" ])

    it "two single line elements should concatenate into a single line" $
      adjoinLines ([ Line "a" ], [ Line "b" ]) ([ Line "a" ], [ Line "b" ]) `shouldBe` ([ Line "aa" ], [ Line "bb" ])
