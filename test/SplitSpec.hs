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
import qualified Data.Maybe as Maybe
import Patch
import Syntax
import ArbitraryTerm

instance Arbitrary a => Arbitrary (Row a) where
  arbitrary = oneof [
    Row <$> arbitrary <*> arbitrary ]

instance Arbitrary HTML where
  arbitrary = oneof [
    Text <$> arbitrary,
    Span <$> arbitrary <*> arbitrary,
    const Break <$> (arbitrary :: Gen ()) ]

instance Arbitrary a => Arbitrary (Line a) where
  arbitrary = oneof [
    Line <$> arbitrary,
    const EmptyLine <$> (arbitrary :: Gen ()) ]

arbitraryLeaf :: Gen (String, Info, Syntax String f)
arbitraryLeaf = toTuple <$> arbitrary
  where toTuple string = (string, Info (Range 0 $ length string) mempty, Leaf string)

spec :: Spec
spec = do
  describe "splitAnnotatedByLines" $ do
    prop "outputs one row for single-line unchanged leaves" $
      forAll (arbitraryLeaf `suchThat` isOnSingleLine) $
        \ (source, info@(Info range categories), syntax) -> splitAnnotatedByLines (source, source) (range, range) (categories, categories) syntax `shouldBe` [
          Row (Line [ Free $ Annotated info $ Leaf source ]) (Line [ Free $ Annotated info $ Leaf source ]) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` \ s -> filter (/= '\n') s == s) $
          \ source -> splitAnnotatedByLines (source, source) (totalRange source, totalRange source) (mempty, mempty) (Indexed [] :: Syntax String (Diff String Info)) `shouldBe` [
            Row (Line [ Free $ Annotated (Info (totalRange source) mempty) $ Indexed [] ]) (Line [ Free $ Annotated (Info (totalRange source) mempty) $ Indexed [] ]) ]

    prop "preserves line counts in equal sources" $
      \ source -> let range = totalRange source in
        length (splitAnnotatedByLines (source, source) (totalRange source, totalRange source) (mempty, mempty) (Indexed . fst $ foldl combineIntoLeaves ([], 0) source)) `shouldBe` length (filter (== '\n') source) + 1

  describe "annotatedToRows" $ do
    it "outputs one row for single-line empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[]" "branch" (Indexed [])) "[]" "[]" `shouldBe` [ Row (Line [ Ul (Just "category-branch") [ Text "[]" ] ]) (Line [ Ul (Just "category-branch") [ Text "[]" ] ]) ]

    it "outputs one row for single-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a, b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a, b ]" `shouldBe` [ Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) ]

    it "outputs one row for single-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a, b ]" "[ a,  b ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 6 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a, b ]" "[ a,  b ]" `shouldBe` [ Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ", ", span "b", Text " ]" ] ]) (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",  ", span "b", Text " ]" ] ]) ]

    it "outputs two rows for two-line non-empty unchanged indexed nodes" $
      annotatedToRows (unchanged "[ a,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[ a,\nb ]" `shouldBe`
      [
          Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break ] ])
              (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break] ]),
          Row (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
              (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
      ]

    it "outputs two rows for two-line non-empty formatted indexed nodes" $
      annotatedToRows (formatted "[ a,\nb ]" "[\na,\nb ]" "branch" (Indexed [
        Free . offsetAnnotated 2 2 $ unchanged "a" "leaf" (Leaf ""),
        Free . offsetAnnotated 5 5 $ unchanged "b" "leaf" (Leaf "")
      ])) "[ a,\nb ]" "[\na,\nb ]" `shouldBe`
      [
          Row (Line [ Ul (Just "category-branch") [ Text "[ ", span "a", Text ",", Break ] ])
              (Line [ Ul (Just "category-branch") [ Text "[", Break ] ]),
          Row EmptyLine
              (Line [ Ul (Just "category-branch") [ span "a", Text ",", Break ] ]),
          Row (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
              (Line [ Ul (Just "category-branch") [ span "b", Text " ]" ] ])
      ]

    it "" $
      let (sourceA, sourceB) = ("[\na\n,\nb]", "[a,b]") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Free . offsetAnnotated 2 1 $ unchanged "a" "leaf" (Leaf ""),
          Free . offsetAnnotated 6 3 $ unchanged "b" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        [
            Row (Line [ Ul (Just "category-branch") [ Text "[", Break ] ])
                (Line [ Ul (Just "category-branch") [ Text "[", span "a", Text ",", span "b", Text "]" ] ]),
            Row (Line [ Ul (Just "category-branch") [ span "a", Break ] ])
                EmptyLine,
            Row (Line [ Ul (Just "category-branch") [ Text ",", Break ] ])
                EmptyLine,
            Row (Line [ Ul (Just "category-branch") [ span "b", Text "]" ] ])
                EmptyLine
        ]

    it "splits multi-line deletions across multiple rows" $
      let (sourceA, sourceB) = ("/*\n*/\na", "a") in
        annotatedToRows (formatted sourceA sourceB "branch" (Indexed [
          Pure . Delete $ (Info (Range 0 5) (Set.fromList ["leaf"]) :< Leaf ""),
          Free . offsetAnnotated 6 0 $ unchanged "a" "leaf" (Leaf "")
        ])) sourceA sourceB `shouldBe`
        [
          Row (Line [ Ul (Just "category-branch") [ Div (Just "delete") [ span "/*", Break ] ] ]) EmptyLine,
          Row (Line [ Ul (Just "category-branch") [ Div (Just "delete") [ span "*/" ], Break ] ]) EmptyLine,
          Row (Line [ Ul (Just "category-branch") [ span "a" ] ]) (Line [ Ul (Just "category-branch") [ span "a" ] ])
        ]

    describe "unicode" $
      it "equivalent precomposed and decomposed characters are not equal" $
        let (sourceA, sourceB) = ("t\776", "\7831")
            syntax = Leaf . Pure $ Replace (info sourceA "leaf" :< Leaf "") (info sourceB "leaf" :< Leaf "")
        in
            annotatedToRows (formatted sourceA sourceB "leaf" syntax) sourceA sourceB `shouldBe`
            [ Row (Line [ span "t\776" ]) (Line [ span "\7831"]) ]


  describe "adjoinRowsBy" $ do
    prop "is identity on top of no rows" $
      \ a -> adjoinRowsBy openElement openElement [] a == [ a ]

    prop "appends onto open rows" $
      forAll ((arbitrary `suchThat` isOpen) >>= \ a -> (,) a <$> (arbitrary `suchThat` isOpen)) $
        \ (a@(Row (Line a1) (Line b1)), b@(Row (Line a2) (Line b2))) ->
          adjoinRowsBy openElement openElement [ a ] b `shouldBe` [ Row (Line $ a1 ++ a2) (Line $ b1 ++ b2) ]

    prop "does not append onto closed rows" $
      forAll ((arbitrary `suchThat` isClosed) >>= \ a -> (,) a <$> (arbitrary `suchThat` isClosed)) $
        \ (a, b) -> adjoinRowsBy openElement openElement [ a ] b `shouldBe` [ b, a ]

    prop "does not promote elements through empty lines onto closed lines" $
      forAll ((arbitrary `suchThat` isClosed) >>= \ a -> (,) a <$> (arbitrary `suchThat` isClosed)) $
        \ (a, b) -> adjoinRowsBy openElement openElement [ Row EmptyLine EmptyLine, a ] b `shouldBe` [ b, Row EmptyLine EmptyLine, a ]

    prop "promotes elements through empty lines onto open lines" $
      forAll ((arbitrary `suchThat` isOpen) >>= \ a -> (,) a <$> (arbitrary `suchThat` isOpen)) $
        \ (a, b) -> adjoinRowsBy openElement openElement [ Row EmptyLine EmptyLine, a ] b `shouldBe` Row EmptyLine EmptyLine : adjoinRowsBy openElement openElement [ a ] b

  describe "termToLines" $ do
    it "splits multi-line terms into multiple lines" $
      termToLines (Info (Range 0 5) (Set.singleton "leaf") :< Leaf "") "/*\n*/"
      `shouldBe`
      ([
        Line [ span "/*", Break ],
        Line [ span "*/" ]
      ], Range 0 5)

  describe "splitTermByLines" $ do
    it "splits multi-line terms into multiple lines" $
      splitTermByLines (Info (Range 0 5) mempty :< Leaf "") "/*\n*/"
      `shouldBe`
      ([
        Line [ Info (Range 0 3) mempty :< Leaf "" ],
        Line [ Info (Range 3 5) mempty :< Leaf "" ]
      ], Range 0 5)

  describe "openLineBy" $ do
    it "produces the earliest non-empty line in a list, if open" $
      openLineBy openElement [
        Line [ Div (Just "delete") [ span "*/" ] ],
        Line [ Div (Just "delete") [ span " * Debugging", Break ] ],
        Line [ Div (Just "delete") [ span "/*", Break ] ]
      ] `shouldBe` (Just $ Line [ Div (Just "delete") [ span "*/" ] ])

    it "produces the earliest non-empty line in a list, if open" $
      openLineBy (openTerm "\n ") [
        Line [ Info (Range 1 2) mempty :< Leaf "" ],
        Line [ Info (Range 0 1) mempty :< Leaf "" ]
      ] `shouldBe` (Just $ Line [ Info (Range 1 2) mempty :< Leaf "" ])

    it "returns Nothing if the earliest non-empty line is closed" $
      openLineBy openElement [
        Line [ Div (Just "delete") [ span " * Debugging", Break ] ]
      ] `shouldBe` Nothing

    it "returns Nothing if the earliest non-empty line is closed" $
      openLineBy (openTerm "\n") [
        Line [ Info (Range 0 1) mempty :< Leaf "" ]
      ] `shouldBe` Nothing

  describe "openTerm" $ do
    it "returns Just the term if its substring does not end with a newline" $
      let term = Info (Range 0 2) mempty :< Leaf "" in openTerm "  " term `shouldBe` Just term

    it "returns Nothing for terms whose substring ends with a newline" $
      openTerm " \n" (Info (Range 0 2) mempty :< Leaf "") `shouldBe` Nothing

    where
      rightRowText text = rightRow [ Text text ]
      rightRow xs = Row EmptyLine (Line xs)
      leftRowText text = leftRow [ Text text ]
      leftRow xs = Row (Line xs) EmptyLine
      rowText a b = Row (Line [ Text a ]) (Line [ Text b ])
      info source category = Info (totalRange source)  (Set.fromList [ category ])
      unchanged source = formatted source source
      formatted source1 source2 category = Annotated (info source1 category, info source2 category)
      offsetInfo by (Info (Range start end) categories) = Info (Range (start + by) (end + by)) categories
      offsetAnnotated by1 by2 (Annotated (left, right) syntax) = Annotated (offsetInfo by1 left, offsetInfo by2 right) syntax
      span = Span (Just "category-leaf")
      isOpen (Row a b) = Maybe.isJust (openLineBy openElement [ a ]) && Maybe.isJust (openLineBy openElement [ b ])
      isClosed (Row a@(Line _) b@(Line _)) = Maybe.isNothing (openLineBy openElement [ a ]) && Maybe.isNothing (openLineBy openElement [ b ])
      isClosed (Row _ _) = False

      isOnSingleLine (a, _, _) = filter (/= '\n') a == a

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info (Range start $ start + 1) mempty, Info (Range start $ start + 1) mempty) (Leaf [ char ]) ], start + 1)
