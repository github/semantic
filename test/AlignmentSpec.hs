module AlignmentSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)
import Data.Text.Arbitrary ()

import Alignment
import ArbitraryTerm ()
import Control.Comonad.Cofree
import Control.Monad.Free hiding (unfold)
import Data.Copointed
import Data.Functor.Both as Both
import Diff
import qualified Data.Maybe as Maybe
import Data.Functor.Identity
import Line
import Prelude hiding (fst, snd)
import qualified Prelude
import Row
import Range
import Source hiding ((++))
import Syntax

instance Arbitrary a => Arbitrary (Both a) where
  arbitrary = pure (curry Both) <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Row a) where
  arbitrary = Row <$> arbitrary

instance Arbitrary a => Arbitrary (Line a) where
  arbitrary = oneof [
    makeLine <$> arbitrary,
    const EmptyLine <$> (arbitrary :: Gen ()) ]

instance Arbitrary a => Arbitrary (Source a) where
  arbitrary = fromList <$> arbitrary

arbitraryLeaf :: Gen (Source Char, Info, Syntax (Source Char) f)
arbitraryLeaf = toTuple <$> arbitrary
  where toTuple string = (string, Info (Range 0 $ length string) mempty, Leaf string)

spec :: Spec
spec = parallel $ do
  describe "splitAnnotatedByLines" $ do
    prop "outputs one row for single-line unchanged leaves" $
      forAll (arbitraryLeaf `suchThat` isOnSingleLine) $
        \ (source, info@(Info range categories), syntax) -> splitAnnotatedByLines (pure source) (pure range) (pure categories) syntax `shouldBe` [
          makeRow (makeLine [ Free $ Annotated info $ Leaf source ]) (makeLine [ Free $ Annotated info $ Leaf source ]) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` (\ a -> filter (/= '\n') (toList a) == toList a)) $
          \ source -> splitAnnotatedByLines (pure source) (pure (getTotalRange source)) (pure mempty) (Indexed [] :: Syntax String (Diff String Info)) `shouldBe` [
            makeRow (makeLine [ Free $ Annotated (Info (getTotalRange source) mempty) $ Indexed [] ]) (makeLine [ Free $ Annotated (Info (getTotalRange source) mempty) $ Indexed [] ]) ]

    prop "preserves line counts in equal sources" $
      \ source ->
        length (splitAnnotatedByLines (pure source) (pure (getTotalRange source)) (pure mempty) (Indexed . Prelude.fst $ foldl combineIntoLeaves ([], 0) source)) `shouldBe` length (filter (== '\n') $ toList source) + 1

    prop "produces the maximum line count in inequal sources" $
      \ sources ->
        length (splitAnnotatedByLines sources (getTotalRange <$> sources) (pure mempty) (Indexed $ leafWithRangesInSources sources <$> Both.zip (actualLineRanges <$> (getTotalRange <$> sources) <*> sources))) `shouldBe` runBothWith max ((+ 1) . length . filter (== '\n') . toList <$> sources)

  describe "adjoinRowsBy" $ do
    prop "is identity on top of no rows" $
      \ a -> adjoinRowsBy (pure openMaybe) [] a == [ a ]

    prop "appends onto open rows" $
      forAll ((arbitrary `suchThat` isOpenBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isOpenBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) [ a ] b `shouldBe` [ Row (mappend <$> unRow a <*> unRow b) ]

    prop "does not append onto closed rows" $
      forAll ((arbitrary `suchThat` isClosedBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isClosedBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) [ a ] b `shouldBe` [ b, a ]

    prop "does not promote elements through empty lines onto closed lines" $
      forAll ((arbitrary `suchThat` isClosedBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isClosedBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) [ makeRow EmptyLine EmptyLine, a ] b `shouldBe` [ b, makeRow EmptyLine EmptyLine, a ]

    prop "promotes elements through empty lines onto open lines" $
      forAll ((arbitrary `suchThat` isOpenBy openMaybe) >>= \ a -> (,) a <$> (arbitrary `suchThat` isOpenBy openMaybe)) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) [ makeRow EmptyLine EmptyLine, a ] b `shouldBe` makeRow EmptyLine EmptyLine : adjoinRowsBy (pure openMaybe) [ a ] b

  describe "splitAbstractedTerm" $ do
    prop "preserves line count" $
      \ source -> let range = getTotalRange source in
        splitAbstractedTerm copoint unwrap (:<) source (Info range mempty :< Leaf source) `shouldBe` (pure . (:< Leaf source) . (`Info` mempty) <$> actualLineRanges range source, range)

  describe "openLineBy" $ do
    it "produces the earliest non-empty line in a list, if open" $
      openLineBy openMaybe [
        pure (Just True),
        pure (Just False)
      ] `shouldBe` (Just $ pure $ Just True)

    it "returns Nothing if the earliest non-empty line is closed" $
      openLineBy openMaybe [
        pure Nothing, pure (Just True)
      ] `shouldBe` Nothing

    where
      isOpenBy f (Row lines) = and (Maybe.isJust . openLineBy f . pure <$> lines)
      isClosedBy f (Row lines@(Both (Line _, Line _))) = and (Maybe.isNothing . openLineBy f . pure <$> lines)
      isClosedBy _ _ = False

      isOnSingleLine (a, _, _) = filter (/= '\n') (toList a) == toList a

      getTotalRange (Source vector) = Range 0 $ length vector

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info <$> (pure (Range start $ start + 1)) <*> mempty) (Leaf [ char ]) ], start + 1)

      leafWithRangesInSources sources ranges = Free $ Annotated (Info <$> ranges <*> pure mempty) (Leaf $ toList (fst sources) ++ toList (snd sources))

      openMaybe :: Maybe Bool -> Maybe (Maybe Bool)
      openMaybe (Just a) = Just (Just a)
      openMaybe Nothing = Nothing
