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
          Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break ] ])
              (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break] ]),
          Row (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
              (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
       ], (Range 0 8, Range 0 8))

    it "outputs two rows for two-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a,\nb ]" "[\na,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[\na,\nb ]" `shouldBe`
      ([
          Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break ] ])
              (Line [ Ul (Just "category-branch") [ Text "[", Break ] ]),
          Row EmptyLine
              (Line [ Ul (Just "category-branch") [ span "a", Text ",", Break ] ]),
          Row (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
              (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
       ], (Range 0 8, Range 0 8))

    it "" $
      let (sourceA, sourceB) = ("[\na\n,\nb]", "[a,b]") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Free . offsetAnnotated 2 1 $ unchanged "a" "leaf" (Leaf ""),
          Free . offsetAnnotated 6 3 $ unchanged "b" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        ([
            Row (Line [ Ul (Just "category-branch") [ Text "[", Break ] ])
                (Line [ Ul (Just "category-branch") [ Text "[", span "a", Text ",", span "b", Text "]" ] ]),
            Row (Line [ Ul (Just "category-branch") [ span "a", Break ] ])
                EmptyLine,
            Row (Line [ Ul (Just "category-branch") [ Text ",", Break ] ])
                EmptyLine,
            Row (Line [ Ul (Just "category-branch") [ span "b", Text "]" ] ])
                EmptyLine
        ], (Range 0 8, Range 0 5))

  describe "adjoin2" $ do
    it "appends a right-hand line without newlines" $
      adjoin2 [ rightRowText "[" ] (rightRowText "a") `shouldBe`
              [ rightRow [ Text "[", Text "a" ] ]

    it "does not append onto lines ending in breaks" $
      adjoin2 [ leftRow [ Break ] ] (leftRowText ",") `shouldBe`
              [ leftRowText ",", leftRow [ Break ]  ]

    it "produces new rows for newlines" $
      adjoin2 [ leftRowText "a" ] (leftRow  [ Break ]) `shouldBe`
              [ leftRow [ Text "a", Break ] ]

    it "does not promote HTML through empty lines onto complete lines" $
      adjoin2 [ rightRowText "b", leftRow [ Break ] ] (leftRowText "a") `shouldBe`
              [ leftRowText "a", rightRowText "b", leftRow [ Break ] ]

    it "promotes breaks through empty lines onto incomplete lines" $
      adjoin2 [ rightRowText "c", rowText "a" "b" ] (leftRow [ Break ]) `shouldBe`
        [ rightRowText "c", Row (Line [ Text "a", Break ]) (Line [ Text "b" ]) ]

    where
      rightRowText text = rightRow [ Text text ]
      rightRow xs = Row EmptyLine (Line xs)
      leftRowText text = leftRow [ Text text ]
      leftRow xs = Row (Line xs) EmptyLine
      rowText a b = Row (Line [ Text a ]) (Line [ Text b ])
      info source category = Info (totalRange source) (Range 0 0) (Set.fromList [ category ])
      unchanged source category = formatted source source category
      formatted source1 source2 category = Annotated (info source1 category, info source2 category)
      offsetInfo by (Info (Range start end) lineRange categories) = Info (Range (start + by) (end + by)) lineRange categories
      offsetAnnotated by1 by2 (Annotated (left, right) syntax) = Annotated (offsetInfo by1 left, offsetInfo by2 right) syntax
      span = Span (Just "category-leaf")
