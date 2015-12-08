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
      adjoinRows [ Row EmptyLine EmptyLine ] [ Row (Line [ Text "a" ]) (Line [ Text "b" ]) ] `shouldBe` [ Row (Line [ Text "a" ]) (Line [ Text "b" ]) ]

    it "empty lines are the left unit for multiple lines" $
      adjoinRows [ Row EmptyLine EmptyLine ] [ Row (Line [ Text "a" ]) (Line [ Text "b" ]), Row (Line [ Text "a" ]) (Line [ Text "b" ]) ] `shouldBe` [ Row (Line [ Text "a" ]) (Line [ Text "b" ]), Row (Line [ Text "a" ]) (Line [ Text "b" ]) ]

    it "two single line elements should concatenate into a single line" $
      adjoinRows [ Row (Line [ Text "a" ]) (Line [ Text "b" ]) ] [ Row (Line [ Text "a" ]) (Line [ Text "b" ]) ] `shouldBe` [ Row (Line [ Text "a", Text "a" ]) (Line [ Text "b", Text "b" ]) ]

    it "single line elements on the left concatenate onto the first of multiple lines on the right" $
      adjoinRows [ Row (Line [ Text "a1" ]) (Line [ Text "b1" ]) ] [ Row (Line [ Text "a2" ]) (Line [ Text "b2" ]), Row (Line [ Text "a3" ]) (Line [ Text "b3" ]) ] `shouldBe` [ Row (Line [ Text "a1", Text "a2" ]) (Line [ Text "b1", Text "b2" ]), Row (Line [ Text "a3" ]) (Line [ Text "b3" ]) ]

    it "the last of multiple line elements on the left concatenate onto the first of multiple lines on the right" $
      adjoinRows [ Row (Line [ Text "a1" ]) (Line [ Text "b1" ]), Row (Line [ Text "a2" ]) (Line [ Text "b2" ]) ]
                 [ Row (Line [ Text "a3" ]) (Line [ Text "b3" ]), Row (Line [ Text "a4" ]) (Line [ Text "b4" ]) ]
      `shouldBe`
      [ Row (Line [ Text "a1" ]) (Line [ Text "b1" ]),
        Row (Line [ Text "a2", Text "a3" ]) (Line [ Text "b2", Text "b3" ]),
        Row (Line [ Text "a4" ]) (Line [ Text "b4" ]) ]


  describe "annotatedToRows" $ do
    it "outputs one row for single-line unchanged leaves" $
      annotatedToRows (unchanged "a" "leaf" (Leaf "")) "a" "a" `shouldBe` ([ Row (Line [ span "a" ]) (Line [ span "a" ]) ], (Range 0 1, Range 0 1))

    it "outputs one row for single-line empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[]" "branch" (Indexed [])) "[]" "[]" `shouldBe` ([ Row (Line [ Ul (Just "category-branch") [ Text "[]" ] ]) (Line [ Ul (Just "category-branch") [ Text "[]" ] ]) ], (Range 0 2, Range 0 2))

    it "outputs one row for single-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a, b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a, b ]" `shouldBe` ([ Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) ], (Range 0 8, Range 0 8))

    it "outputs one row for single-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a, b ]" "[ a,  b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 6 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a,  b ]" `shouldBe` ([ Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",  ", span "b", Text " ]" ] ]) ], (Range 0 8, Range 0 9))

    it "outputs two rows for two-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[ a,\nb ]" `shouldBe`
      ([
          Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text "," ] ])
              (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text "," ] ]),
          Row (Line [ Ul (Just "category-branch") [ Text "", span "b", Text " ]" ] ])
              (Line [ Ul (Just "category-branch") [ Text "", span "b", Text " ]" ] ])
       ], (Range 0 8, Range 0 8))

    it "outputs two rows for two-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a,\nb ]" "[\na,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[\na,\nb ]" `shouldBe`
      ([
          Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text "," ] ])
              (Line [ Ul (Just "category-branch") [ Text "[" ] ]),
          Row EmptyLine
              (Line [ Ul (Just "category-branch") [ Text "", span "a", Text "," ] ]),
          Row (Line [ Ul (Just "category-branch") [ Text "", span "b", Text " ]" ] ])
              (Line [ Ul (Just "category-branch") [ Text "", span "b", Text " ]" ] ])
       ], (Range 0 8, Range 0 8))

    it "" $
      let (sourceA, sourceB) = ("[\na\n,\nb]", "[a,b]") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Free . offsetAnnotated 2 1 $ unchanged "a" "leaf" (Leaf ""),
          Free . offsetAnnotated 7 3 $ unchanged "b" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        ([
            Row (Line [ Ul (Just "category-branch") [ Text "[" ] ])
                (Line [ Ul (Just "category-branch") [ Text "[", span "a", Text ",", span "b", Text "]" ] ]),
            Row (Line [ Ul (Just "category-branch") [ Text "", span "a" ] ])
                EmptyLine,
            Row (Line [ Ul (Just "category-branch") [ Text "", Text "," ] ])
                EmptyLine,
            Row (Line [ Ul (Just "category-branch") [ Text "", span "b", Text " ]" ] ])
                EmptyLine
        ], (Range 0 8, Range 0 5))

  describe "adjoin2" $ do
    it "appends a row starting with a newline" $
      adjoin2 [ Row (Line [ Ul Nothing [ Text "[",Span Nothing "a" ]]) EmptyLine ] (Row (Line [ Text "", Text "," ]) EmptyLine) `shouldBe`
      [ Row (Line [ Text "", Text "," ]) EmptyLine, Row (Line [ Ul Nothing [ Text "[",Span Nothing "a" ]]) EmptyLine ]

    where
      info source category = Info (totalRange source) (Range 0 0) (Set.fromList [ category ])
      unchanged source category = formatted source source category
      formatted source1 source2 category = Annotated (info source1 category, info source2 category)
      offsetInfo by (Info (Range start end) lineRange categories) = Info (Range (start + by) (end + by)) lineRange categories
      offsetAnnotated by1 by2 (Annotated (left, right) syntax) = Annotated (offsetInfo by1 left, offsetInfo by2 right) syntax
      span = Span (Just "category-leaf")
