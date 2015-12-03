module Main where

import Split
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "split" $ do
    it "empty lines are the left unit" $
      adjoinRows [ Row [] [] ] [ Row [ Text "a" ] [ Text "b" ] ] `shouldBe` [ Row [ Text "a" ] [ Text "b" ] ]

    it "empty lines are the left unit for multiple lines" $
      adjoinRows [ Row [] [] ] [ Row [ Text "a" ] [ Text "b" ], Row [ Text "a" ] [ Text "b" ] ] `shouldBe` [ Row [ Text "a" ] [ Text "b" ], Row [ Text "a" ] [ Text "b" ] ]

    it "two single line elements should concatenate into a single line" $
      adjoinRows [ Row [ Text "a" ] [ Text "b" ] ] [ Row [ Text "a" ] [ Text "b" ] ] `shouldBe` [ Row [ Text "a", Text "a" ] [ Text "b", Text "b" ] ]

    it "single line elements on the left concatenate onto the first of multiple lines on the right" $
      adjoinRows [ Row [ Text "a1" ] [ Text "b1" ] ] [ Row [ Text "a2" ] [ Text "b2" ], Row [ Text "a3" ] [ Text "b3" ] ] `shouldBe` [ Row [ Text "a1", Text "a2" ] [ Text "b1", Text "b2" ], Row [ Text "a3" ] [ Text "b3" ] ]

    it "the last of multiple line elements on the left concatenate onto the first of multiple lines on the right" $
      adjoinRows [ Row [ Text "a1" ] [ Text "b1" ], Row [ Text "a2" ] [ Text "b2" ] ] [ Row [ Text "a3" ] [ Text "b3" ], Row [ Text "a4" ] [ Text "b4" ] ] `shouldBe` [ Row [ Text "a1" ] [ Text "b1" ], Row [ Text "a2", Text "a3" ] [ Text "b2", Text "b3" ], Row [ Text "a4" ] [ Text "b4" ] ]
