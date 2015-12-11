module Main where

import Diff
import Patch
import Range
import Split
import Syntax
import Term
import Control.Comonad.Cofree
import Control.Monad.Free
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype ArbitraryTerm = ArbitraryTerm (Term String ())
  deriving (Show, Eq)

instance Arbitrary ArbitraryTerm where
  arbitrary = oneof [ ArbitraryTerm . (() :<) . Leaf <$> arbitrary ]

instance Arbitrary HTML where
  arbitrary = oneof [
    Text <$> arbitrary,
    Span <$> arbitrary <*> arbitrary,
    const Break <$> (arbitrary :: Gen ()) ]

instance Arbitrary Line where
  arbitrary = oneof [
    Line <$> arbitrary,
    const EmptyLine <$> (arbitrary :: Gen ()) ]

instance Arbitrary Row where
  arbitrary = oneof [
    Row <$> arbitrary <*> arbitrary ]

main :: IO ()
main = hspec $ do
  describe "Term" $ do
    prop "equality is reflexive" $
      \ a -> a == (a :: ArbitraryTerm)

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

    it "should split multi-line deletions across multiple rows" $
      let (sourceA, sourceB) = ("/*\n*/\na", "a") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Pure . Delete $ (Info (Range 0 5) (Range 0 2) (Set.fromList ["leaf"]) :< (Leaf "")),
          Free . offsetAnnotated 6 0 $ unchanged "a" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        ([
          Row (Line [ Ul (Just "category-branch") [ Div (Just "delete") [ span "/*", Break ] ] ]) EmptyLine,
          Row (Line [ Ul (Just "category-branch") [ Div (Just "delete") [ span "*/" ], Break ] ]) EmptyLine,
          Row (Line [ Ul (Just "category-branch") [ span "a" ] ]) (Line [ Ul (Just "category-branch") [ span "a" ] ])
        ], (Range 0 7, Range 0 1))

  describe "adjoin2" $ do
    prop "is idempotent for additions of empty rows" $
      \ a -> adjoin2 (adjoin2 [ a ] mempty) mempty == (adjoin2 [ a ] mempty)

    prop "is identity on top of empty rows" $
      \ a -> adjoin2 [ mempty ] a == [ a ]

    prop "is identity on top of no rows" $
      \ a -> adjoin2 [] a == [ a ]

    it "appends appends HTML onto incomplete lines" $
      adjoin2 [ rightRowText "[" ] (rightRowText "a") `shouldBe`
              [ rightRow [ Text "[", Text "a" ] ]

    it "does not append HTML onto complete lines" $
      adjoin2 [ leftRow [ Break ] ] (leftRowText ",") `shouldBe`
              [ leftRowText ",", leftRow [ Break ]  ]

    it "appends breaks onto incomplete lines" $
      adjoin2 [ leftRowText "a" ] (leftRow  [ Break ]) `shouldBe`
              [ leftRow [ Text "a", Break ] ]

    it "does not promote HTML through empty lines onto complete lines" $
      adjoin2 [ rightRowText "b", leftRow [ Break ] ] (leftRowText "a") `shouldBe`
              [ leftRowText "a", rightRowText "b", leftRow [ Break ] ]

    it "promotes breaks through empty lines onto incomplete lines" $
      adjoin2 [ rightRowText "c", rowText "a" "b" ] (leftRow [ Break ]) `shouldBe`
        [ rightRowText "c", Row (Line [ Text "a", Break ]) (Line [ Text "b" ]) ]

  describe "termToLines" $ do
    it "splits multi-line terms into multiple lines" $
      termToLines (Info (Range 0 5) (Range 0 2) (Set.singleton "leaf") :< (Leaf "")) "/*\n*/"
      `shouldBe`
      ([
        Line [ span "/*", Break ],
        Line [ span "*/" ]
      ], Range 0 5)

  describe "openLine" $ do
    it "should produce the earliest non-empty line in a list, if open" $
      openLine [
        Line [ Div (Just "delete") [ span "*/" ] ],
        Line [ Div (Just "delete") [ span " * Debugging", Break ] ],
        Line [ Div (Just "delete") [ span "/*", Break ] ]
      ] `shouldBe` (Just $ Line [ Div (Just "delete") [ span "*/" ] ])

    it "should return Nothing if the earliest non-empty line is closed" $
      openLine [
        Line [ Div (Just "delete") [ span " * Debugging", Break ] ]
      ] `shouldBe` Nothing

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
