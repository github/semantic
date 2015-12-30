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
import Data.Functor.Identity
import Source hiding ((++))
import Line
import Row
import Patch
import Syntax
import ArbitraryTerm

instance Arbitrary a => Arbitrary (Row a) where
  arbitrary = oneof [
    Row <$> arbitrary <*> arbitrary ]

instance Arbitrary a => Arbitrary (Line a) where
  arbitrary = oneof [
    makeLine <$> arbitrary,
    const EmptyLine <$> (arbitrary :: Gen ()) ]

instance Arbitrary a => Arbitrary (Source a) where
  arbitrary = fromList <$> arbitrary

arbitraryLeaf :: Gen (Source Char, Info, Syntax String f)
arbitraryLeaf = toTuple <$> arbitrary
  where toTuple source = (source, Info (Range 0 $ length source) mempty, Leaf (toString source))

spec :: Spec
spec = do
  describe "splitAnnotatedByLines" $ do
    prop "outputs one row for single-line unchanged leaves" $
      forAll (arbitraryLeaf `suchThat` isOnSingleLine) $
        \ (source, info@(Info range categories), syntax) -> splitAnnotatedByLines (source, source) (range, range) (categories, categories) syntax `shouldBe` [
          Row (makeLine [ Free $ Annotated info $ Leaf (toString source) ]) (makeLine [ Free $ Annotated info $ Leaf (toString source) ]) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` (\ a -> filter (/= '\n') (toList a) == toList a)) $
          \ source -> splitAnnotatedByLines (source, source) (getTotalRange source, getTotalRange source) (mempty, mempty) (Indexed [] :: Syntax String (Diff String Info)) `shouldBe` [
            Row (makeLine [ Free $ Annotated (Info (getTotalRange source) mempty) $ Indexed [] ]) (makeLine [ Free $ Annotated (Info (getTotalRange source) mempty) $ Indexed [] ]) ]

    prop "preserves line counts in equal sources" $
      \ source ->
        length (splitAnnotatedByLines (source, source) (getTotalRange source, getTotalRange source) (mempty, mempty) (Indexed . fst $ foldl combineIntoLeaves ([], 0) source)) `shouldBe` length (filter (== '\n') $ toList source) + 1

    prop "produces the maximum line count in inequal sources" $
      \ sourceA sourceB ->
        length (splitAnnotatedByLines (sourceA, sourceB) (getTotalRange sourceA, getTotalRange sourceB) (mempty, mempty) (Indexed $ zipWith (leafWithRangesInSources sourceA sourceB) (actualLineRanges (getTotalRange sourceA) sourceA) (actualLineRanges (getTotalRange sourceB) sourceB))) `shouldBe` max (length (filter (== '\n') $ toList sourceA) + 1) (length (filter (== '\n') $ toList sourceB) + 1)

  describe "adjoinRowsBy" $ do
    prop "is identity on top of no rows" $
      \ a -> adjoinRowsBy openMaybe openMaybe [] a == [ a ]

    prop "appends onto open rows" $
      forAll ((arbitrary `suchThat` isOpenBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isOpenBy openMaybe)) $
        \ (a@(Row a1 b1), b@(Row a2 b2)) ->
          adjoinRowsBy openMaybe openMaybe [ a ] b `shouldBe` [ Row (makeLine $ unLine a1 ++ unLine a2) (makeLine $ unLine b1 ++ unLine b2) ]

    prop "does not append onto closed rows" $
      forAll ((arbitrary `suchThat` isClosedBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isClosedBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy openMaybe openMaybe [ a ] b `shouldBe` [ b, a ]

    prop "does not promote elements through empty lines onto closed lines" $
      forAll ((arbitrary `suchThat` isClosedBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isClosedBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy openMaybe openMaybe [ Row EmptyLine EmptyLine, a ] b `shouldBe` [ b, Row EmptyLine EmptyLine, a ]

    prop "promotes elements through empty lines onto open lines" $
      forAll ((arbitrary `suchThat` isOpenBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isOpenBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy openMaybe openMaybe [ Row EmptyLine EmptyLine, a ] b `shouldBe` Row EmptyLine EmptyLine : adjoinRowsBy openMaybe openMaybe [ a ] b

  describe "splitTermByLines" $ do
    prop "preserves line count" $
      \ source -> let range = getTotalRange source in
        splitTermByLines (Info range mempty :< Leaf (toString source)) source `shouldBe` (pure . (:< Leaf (toString source)) . (`Info` mempty) <$> actualLineRanges range source, range)

  describe "openLineBy" $ do
    it "produces the earliest non-empty line in a list, if open" $
      openLineBy (openTerm $ fromList "\n ") [
        makeLine [ Info (Range 1 2) mempty :< Leaf "" ],
        makeLine [ Info (Range 0 1) mempty :< Leaf "" ]
      ] `shouldBe` (Just $ makeLine [ Info (Range 1 2) mempty :< Leaf "" ])

    it "returns Nothing if the earliest non-empty line is closed" $
      openLineBy (openTerm $ fromList "\n") [
        makeLine [ Info (Range 0 1) mempty :< Leaf "" ]
      ] `shouldBe` Nothing

  describe "openTerm" $ do
    it "returns Just the term if its substring does not end with a newline" $
      let term = Info (Range 0 2) mempty :< Leaf "" in openTerm (fromList "  ") (Identity term) `shouldBe` Just (Identity term)

    it "returns Nothing for terms whose substring ends with a newline" $
      openTerm (fromList " \n") (Identity $ Info (Range 0 2) mempty :< Leaf "") `shouldBe` Nothing

    where
      isOpenBy f (Row a b) = Maybe.isJust (openLineBy f [ a ]) && Maybe.isJust (openLineBy f [ b ])
      isClosedBy f (Row a@(Line _) b@(Line _)) = Maybe.isNothing (openLineBy f [ a ]) && Maybe.isNothing (openLineBy f [ b ])
      isClosedBy _ (Row _ _) = False

      isOnSingleLine (a, _, _) = filter (/= '\n') (toList a) == toList a

      getTotalRange (Source vector) = Range 0 $ length vector

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info (Range start $ start + 1) mempty, Info (Range start $ start + 1) mempty) (Leaf [ char ]) ], start + 1)

      leafWithRangesInSources sourceA sourceB rangeA rangeB = Free $ Annotated (Info rangeA mempty, Info rangeB mempty) (Leaf $ toList sourceA ++ toList sourceB)

      openMaybe :: Maybe a -> Maybe (Maybe a)
      openMaybe (Just a) = Just (Just a)
      openMaybe Nothing = Nothing
