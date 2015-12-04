module Main where

import Diff
import Range
import Split
import Syntax
import Control.Monad.Free
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
      annotatedToRows (unchanged "a" "leaf" (Leaf "")) "a" "a" `shouldBe` ([ Row [ span "a" ] [ span "a" ] ], (Range 0 1, Range 0 1))

    it "outputs one row for single-line empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[]" "branch" (Indexed [])) "[]" "[]" `shouldBe` ([ Row [ Ul (Just "branch") [ Text "[]" ] ] [ Ul (Just "branch") [ Text "[]" ] ] ], (Range 0 2, Range 0 2))

    it "outputs one row for single-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a, b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a, b ]" `shouldBe` ([ Row [ Ul (Just "branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ] [ Ul (Just "branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ] ], (Range 0 8, Range 0 8))
    where
      info source category = Info (totalRange source) (Range 0 0) (Set.fromList [ category ])
      unchanged source category = Annotated (info source category, info source category)
      offsetInfo by (Info (Range start end) lineRange categories) = Info (Range (start + by) (end + by)) lineRange categories
      offsetAnnotated by (Annotated (left, right) syntax) = Annotated (offsetInfo by left, offsetInfo by right) syntax
      span = Span (Just "leaf")
