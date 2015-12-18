module SplitSpec where

import Test.Hspec
import Split
import qualified Data.Set as Set
import Diff
import Range
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)
import Control.Comonad.Cofree
import Control.Monad.Free hiding (unfold)
import Patch
import Syntax

instance Arbitrary Row where
  arbitrary = oneof [
    Row <$> arbitrary <*> arbitrary ]

instance Arbitrary HTML where
  arbitrary = oneof [
    Text <$> arbitrary,
    Span <$> arbitrary <*> arbitrary,
    const Break <$> (arbitrary :: Gen ()) ]

instance Arbitrary Line where
  arbitrary = oneof [
    Line <$> arbitrary <*> arbitrary,
    const EmptyLine <$> (arbitrary :: Gen ()) ]

spec :: Spec
spec = do
  describe "annotatedToRows" $ do
    it "outputs one row for single-line unchanged leaves" $
      annotatedToRows (unchanged "a" "leaf" (Leaf "")) "a" "a" `shouldBe` ([ Row (Line False [ span "a" ]) (Line False [ span "a" ]) ], (Range 0 1, Range 0 1))

    it "outputs one row for single-line empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[]" "branch" (Indexed [])) "[]" "[]" `shouldBe` ([ Row (Line False [ Ul (Just "category-branch") [ Text "[]" ] ]) (Line False [ Ul (Just "category-branch") [ Text "[]" ] ]) ], (Range 0 2, Range 0 2))

    it "outputs one row for single-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a, b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a, b ]" `shouldBe` ([ Row (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) ], (Range 0 8, Range 0 8))

    it "outputs one row for single-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a, b ]" "[ a,  b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 6 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a,  b ]" `shouldBe` ([ Row (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",  ", span "b", Text " ]" ] ]) ], (Range 0 8, Range 0 9))

    it "outputs two rows for two-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[ a,\nb ]" `shouldBe`
      ([
          Row (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break ] ])
              (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break] ]),
          Row (Line False [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
              (Line False [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
       ], (Range 0 8, Range 0 8))

    it "outputs two rows for two-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a,\nb ]" "[\na,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[\na,\nb ]" `shouldBe`
      ([
          Row (Line False [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break ] ])
              (Line False [ Ul (Just "category-branch") [ Text "[", Break ] ]),
          Row EmptyLine
              (Line False [ Ul (Just "category-branch") [ span "a", Text ",", Break ] ]),
          Row (Line False [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
              (Line False [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
       ], (Range 0 8, Range 0 8))

    it "" $
      let (sourceA, sourceB) = ("[\na\n,\nb]", "[a,b]") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Free . offsetAnnotated 2 1 $ unchanged "a" "leaf" (Leaf ""),
          Free . offsetAnnotated 6 3 $ unchanged "b" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        ([
            Row (Line False [ Ul (Just "category-branch") [ Text "[", Break ] ])
                (Line False [ Ul (Just "category-branch") [ Text "[", span "a", Text ",", span "b", Text "]" ] ]),
            Row (Line False [ Ul (Just "category-branch") [ span "a", Break ] ])
                EmptyLine,
            Row (Line False [ Ul (Just "category-branch") [ Text ",", Break ] ])
                EmptyLine,
            Row (Line False [ Ul (Just "category-branch") [ span "b", Text "]" ] ])
                EmptyLine
        ], (Range 0 8, Range 0 5))

    it "should split multi-line deletions across multiple rows" $
      let (sourceA, sourceB) = ("/*\n*/\na", "a") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Pure . Delete $ (Info (Range 0 5) (Set.fromList ["leaf"]) :< (Leaf "")),
          Free . offsetAnnotated 6 0 $ unchanged "a" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        ([
          Row (Line True [ Ul (Just "category-branch") [ Div (Just "delete") [ span "/*", Break ] ] ]) EmptyLine,
          Row (Line True [ Ul (Just "category-branch") [ Div (Just "delete") [ span "*/" ], Break ] ]) EmptyLine,
          Row (Line False [ Ul (Just "category-branch") [ span "a" ] ]) (Line False [ Ul (Just "category-branch") [ span "a" ] ])
        ], (Range 0 7, Range 0 1))

    describe "unicode" $
      it "equivalent precomposed and decomposed characters are not equal" $
        let (sourceA, sourceB) = ("t\776", "\7831")
            syntax = Leaf . Pure $ Replace (info sourceA "leaf" :< (Leaf "")) (info sourceB "leaf" :< (Leaf ""))
        in
            annotatedToRows (formatted sourceA sourceB "leaf" syntax) sourceA sourceB `shouldBe`
            ([ Row (Line False [ span "t\776" ]) (Line False [ span "\7831"]) ], (Range 0 2, Range 0 1))


  describe "adjoin2" $ do
    prop "is idempotent for additions of empty rows" $
      \ a -> adjoin2 (adjoin2 [ a ] mempty) mempty == (adjoin2 [ a ] mempty)

    prop "is identity on top of empty rows" $
      \ a -> adjoin2 [ mempty ] a == [ a ]

    prop "is identity on top of no rows" $
      \ a -> adjoin2 [] a == [ a ]

    prop "appends onto open rows" $
      forAll ((arbitrary `suchThat` isOpen) >>= \ a -> ((,) a) <$> (arbitrary `suchThat` isOpen)) $
        \ (a@(Row (Line ac1 as1) (Line bc1 bs1)), b@(Row (Line ac2 as2) (Line bc2 bs2))) ->
          adjoin2 [ a ] b `shouldBe` [ Row (Line (ac1 || ac2) $ as1 ++ as2) (Line (bc1 || bc2) $ bs1 ++ bs2) ]

    it "does not append elements onto complete lines" $
      adjoin2 [ leftRow [ Break ] ] (leftRowText ",") `shouldBe`
              [ leftRowText ",", leftRow [ Break ]  ]

    it "appends breaks onto incomplete lines" $
      adjoin2 [ leftRowText "a" ] (leftRow  [ Break ]) `shouldBe`
              [ leftRow [ Text "a", Break ] ]

    it "does not promote elements through empty lines onto complete lines" $
      adjoin2 [ rightRowText "b", leftRow [ Break ] ] (leftRowText "a") `shouldBe`
              [ leftRowText "a", rightRowText "b", leftRow [ Break ] ]

    it "promotes breaks through empty lines onto incomplete lines" $
      adjoin2 [ rightRowText "c", rowText "a" "b" ] (leftRow [ Break ]) `shouldBe`
        [ rightRowText "c", Row (Line False [ Text "a", Break ]) (Line False [ Text "b" ]) ]

  describe "termToLines" $ do
    it "splits multi-line terms into multiple lines" $
      termToLines (Info (Range 0 5) (Set.singleton "leaf") :< (Leaf "")) "/*\n*/"
      `shouldBe`
      ([
        Line True [ span "/*", Break ],
        Line True [ span "*/" ]
      ], Range 0 5)

  describe "openLine" $ do
    it "should produce the earliest non-empty line in a list, if open" $
      openLine [
        Line True [ Div (Just "delete") [ span "*/" ] ],
        Line True [ Div (Just "delete") [ span " * Debugging", Break ] ],
        Line True [ Div (Just "delete") [ span "/*", Break ] ]
      ] `shouldBe` (Just $ Line True [ Div (Just "delete") [ span "*/" ] ])

    it "should return Nothing if the earliest non-empty line is closed" $
      openLine [
        Line True [ Div (Just "delete") [ span " * Debugging", Break ] ]
      ] `shouldBe` Nothing

    where
      rightRowText text = rightRow [ Text text ]
      rightRow xs = Row EmptyLine (Line False xs)
      leftRowText text = leftRow [ Text text ]
      leftRow xs = Row (Line False xs) EmptyLine
      rowText a b = Row (Line False [ Text a ]) (Line False [ Text b ])
      info source category = Info (totalRange source)  (Set.fromList [ category ])
      unchanged source category = formatted source source category
      formatted source1 source2 category = Annotated (info source1 category, info source2 category)
      offsetInfo by (Info (Range start end) categories) = Info (Range (start + by) (end + by)) categories
      offsetAnnotated by1 by2 (Annotated (left, right) syntax) = Annotated (offsetInfo by1 left, offsetInfo by2 right) syntax
      span = Span (Just "category-leaf")
      isOpen (Row a b) = (maybe False (const True) $ openLine [ a ]) && (maybe False (const True) $ openLine [ b ])
      isClosed (Row a b) = (maybe True (const False) $ openLine [ a ]) && (maybe True (const False) $ openLine [ b ])
