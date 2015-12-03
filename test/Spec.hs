module Main where

import Split
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "split" $ do
    it "empty lines are the left unit" $
      adjoinLines ([], []) ([ Line "a" ], [ Line "b" ]) `shouldBe` ([ Line "a" ], [ Line "b" ])

    it "empty lines are the left unit for multiple lines" $
      adjoinLines ([], []) ([ Line "a", Line "a" ], [ Line "b", Line "b" ]) `shouldBe` ([ Line "a", Line "a" ], [ Line "b", Line "b" ])

    it "two single line elements should concatenate into a single line" $
      adjoinLines ([ Line "a" ], [ Line "b" ]) ([ Line "a" ], [ Line "b" ]) `shouldBe` ([ Line "aa" ], [ Line "bb" ])

    it "single line elements on the left concatenate onto the first of multiple lines on the right" $
      adjoinLines ([ Line "a1" ], [ Line "b1" ]) ([ Line "a2", Line "a3" ], [ Line "b2", Line "b3" ]) `shouldBe` ([ Line "a1a2", Line "a3" ], [ Line "b1b2", Line "b3" ])

    it "the last of multiple line elements on the left concatenate onto the first of multiple lines on the right" $
      adjoinLines ([ Line "a1", Line "a2" ], [ Line "b1", Line "b2" ]) ([ Line "a3", Line "a4" ], [ Line "b3", Line "b4" ]) `shouldBe` ([ Line "a1", Line "a2a3", Line "a4" ], [ Line "b1", Line "b2b3", Line "b4" ])
