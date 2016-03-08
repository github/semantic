module AlignmentSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)
import Data.Text.Arbitrary ()

import Alignment
import ArbitraryTerm ()
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Free hiding (unfold)
import Data.Copointed
import Data.Functor.Both as Both
import Diff
import qualified Data.Maybe as Maybe
import Data.Functor.Identity
import Line
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Row
import Range
import Source hiding ((++))
import qualified Source
import SplitDiff
import Syntax

instance Arbitrary a => Arbitrary (Both a) where
  arbitrary = pure (curry Both) <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Row a) where
  arbitrary = Row <$> arbitrary

instance Arbitrary a => Arbitrary (Line a) where
  arbitrary = oneof [
    makeLine <$> arbitrary,
    const EmptyLine <$> (arbitrary :: Gen ()) ]

instance Arbitrary a => Arbitrary (Patch a) where
  arbitrary = oneof [
    Insert <$> arbitrary,
    Delete <$> arbitrary,
    Replace <$> arbitrary <*> arbitrary ]

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
        \ (source, info@(Info range categories), syntax) -> splitAnnotatedByLines (pure source) (pure $ Info range categories) syntax `shouldBe` [
          makeRow (pure (Free $ Annotated info $ Leaf source, Range 0 (length source))) (pure (Free $ Annotated info $ Leaf source, Range 0 (length source))) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` (\ a -> filter (/= '\n') (toList a) == toList a)) $
          \ source -> splitAnnotatedByLines (pure source) (pure $ Info (getTotalRange source) mempty) (Indexed [] :: Syntax String [Row (SplitDiff leaf Info, Range)]) `shouldBe` [
            makeRow (pure (Free $ Annotated (Info (getTotalRange source) mempty) $ Indexed [], Range 0 (length source))) (pure (Free $ Annotated (Info (getTotalRange source) mempty) $ Indexed [], Range 0 (length source))) ]

  describe "splitDiffByLines" $ do
    prop "preserves line counts in equal sources" $
      \ source ->
        length (splitDiffByLines (pure source) (Free $ Annotated (pure $ Info (getTotalRange source) mempty) (Indexed . Prelude.fst $ foldl combineIntoLeaves ([], 0) source))) `shouldBe` length (filter (== '\n') $ toList source) + 1

    prop "produces the maximum line count in inequal sources" $
      \ sources ->
        length (splitDiffByLines sources (Free $ Annotated ((`Info` mempty) . getTotalRange <$> sources) (Indexed $ leafWithRangesInSources sources <$> Both.zip (actualLineRanges <$> (getTotalRange <$> sources) <*> sources)))) `shouldBe` runBothWith max ((+ 1) . length . filter (== '\n') . toList <$> sources)

  describe "adjoinRowsBy" $ do
    prop "is identity on top of no rows" $ forAll (arbitrary `suchThat` (not . isEmptyRow)) $
      \ a -> adjoinRowsBy (pure openMaybe) a [] `shouldBe` [ a ]

    prop "prunes empty rows" $
      \ a -> adjoinRowsBy (pure openMaybe) (makeRow EmptyLine EmptyLine) [ a ] `shouldBe` [ a ]

    prop "merges open rows" $
      forAll ((arbitrary `suchThat` isOpenRowBy (pure openMaybe)) >>= \ a -> (,) a <$> arbitrary) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) a [ b ] `shouldBe` [ Row (mappend <$> unRow a <*> unRow b) ]

    prop "prepends closed rows" $
      forAll ((arbitrary `suchThat` isClosedRowBy (pure openMaybe)) >>= \ a -> (,) a <$> arbitrary) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) a [ b ] `shouldBe` [ a, b ]

    prop "does not promote empty lines through closed rows" $
      forAll ((arbitrary `suchThat` (not . isOpenLineBy openMaybe)) >>= \ a -> (,) a <$> arbitrary) $
        \ (a, b) -> adjoinRowsBy (pure openMaybe) (makeRow EmptyLine a) [ makeRow a a, b ] `shouldBe` [ makeRow a a, b ]

    prop "promotes empty lines through open rows" $
      forAll ((arbitrary `suchThat` (\ (a, b) -> isOpenLineBy openMaybe a && not (isOpenLineBy openMaybe b))) >>= \ (a, b) -> (,,) a b <$> arbitrary) $
        \ (open, closed, rest) -> adjoinRowsBy (pure openMaybe) (makeRow EmptyLine open) [ makeRow open closed, rest ] `shouldBe` [ makeRow open closed, makeRow EmptyLine open, rest ]

  describe "splitAbstractedTerm" $ do
    prop "preserves line count" $
      \ source -> let range = getTotalRange source in
        splitAbstractedTerm (:<) source (Info range mempty) (Leaf source) `shouldBe` (pure . ((:< Leaf source) . (`Info` mempty) &&& id) <$> actualLineRanges range source)

  describe "splitPatchByLines" $ do
    prop "starts at initial indices" $
      \ patch sources -> let indices = length <$> sources in
        fmap start . maybeFirst . Maybe.catMaybes <$> Both.unzip (fmap maybeFirst . unRow . fmap Prelude.snd <$> splitPatchByLines ((Source.++) <$> sources <*> sources) (patchWithBoth patch (leafWithRangeInSource <$> sources <*> (Range <$> indices <*> ((2 *) <$> indices))))) `shouldBe` (<$) <$> indices <*> unPatch patch

    where
      isEmptyRow (Row (Both (EmptyLine, EmptyLine))) = True
      isEmptyRow _ = False

      isEmptyLine EmptyLine = True
      isEmptyLine _ = False

      isOnSingleLine (a, _, _) = filter (/= '\n') (toList a) == toList a

      getTotalRange (Source vector) = Range 0 $ length vector

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info <$> pure (Range start $ start + 1) <*> mempty) (Leaf [ char ]) ], start + 1)

      leafWithRangesInSources sources ranges = Free $ Annotated (Info <$> ranges <*> pure mempty) (Leaf $ runBothWith (++) (toList <$> sources))

      leafWithRangeInSource source range = Info range mempty :< Leaf source

      patchWithBoth (Insert ()) = Insert . snd
      patchWithBoth (Delete ()) = Delete . fst
      patchWithBoth (Replace () ()) = runBothWith Replace

      openMaybe :: Maybe Bool -> Maybe (Maybe Bool)
      openMaybe (Just a) = Just (Just a)
      openMaybe Nothing = Nothing
