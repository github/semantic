module Main where

import Diff
import Range
import Split
import Syntax
import qualified Data.Set as Set
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "adjoinRows" $ do
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

  describe "annotatedToRows" $ do
    it "outputs one row for single-line unchanged leaves" $
      annotatedToRows (unchanged "a" "leaf" (Leaf "")) "a" "a" `shouldBe` ([ Row [ Span (Just "leaf") "a" ] [ Span (Just "leaf") "a" ] ], Range 0 1, Range 0 1)
    where
      oneLineUnchangedIndexedSource = "[ a, b, c ]"
      oneLineUnchangedIndexedRange = totalRange oneLineUnchangedIndexedSource
      info source category = Info (totalRange source) (Range 0 0) (Set.fromList [ category ])
      unchanged source category = Annotated (info source category, info source category)
