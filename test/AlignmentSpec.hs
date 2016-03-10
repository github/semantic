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
import Range
import Source hiding ((++))
import qualified Source
import SplitDiff
import Syntax

instance Arbitrary a => Arbitrary (Both a) where
  arbitrary = pure (curry Both) <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Line a) where
  arbitrary = oneof [ Line <$> arbitrary, Closed <$> arbitrary ]

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
  describe "splitDiffByLines" $ do
    prop "preserves line counts in equal sources" $
      \ source ->
        length (splitDiffByLines (pure source) (Free $ Annotated (pure $ Info (totalRange source) mempty) (Indexed . Prelude.fst $ foldl combineIntoLeaves ([], 0) source))) `shouldBe` length (filter (== '\n') $ toList source) + 1

    prop "produces the maximum line count in inequal sources" $
      \ sources ->
        length (splitDiffByLines sources (Free $ Annotated ((`Info` mempty) . totalRange <$> sources) (Indexed $ leafWithRangesInSources sources <$> Both.zip (actualLineRanges <$> (totalRange <$> sources) <*> sources)))) `shouldBe` runBothWith max ((+ 1) . length . filter (== '\n') . toList <$> sources)

  describe "adjoinRows" $ do
    prop "is identity on top of no rows" $ forAll (arbitrary `suchThat` (not . isEmptyRow)) $
      \ a -> adjoinRows alignRows a [] `shouldBe` [ a :: Row Char ]

    prop "prunes empty rows" $
      \ a -> adjoinRows alignRows mempty [ a ] `shouldBe` [ a :: Row Char ]

    prop "merges open rows" $
      \ a b -> adjoinRows alignRows (pure (Line [a])) [ b ] `shouldBe` [ pure (Line [a]) `mappend` b :: Row Char ]

    prop "prepends closed rows" $
      \ a b -> adjoinRows alignRows (pure (Closed [a])) [ b ] `shouldBe` [ pure (Closed [a]), b :: Row Char ]

    it "aligns closed lines" $
      foldr (adjoinRows alignRows) [] (Prelude.zipWith both (pureBy (/= '\n') <$> "[ bar ]\nquux") (pureBy (/= '\n') <$> "[\nbar\n]\nquux")) `shouldBe`
        [ both (Closed "[ bar ]\n") (Closed "[\n")
        , both (Closed "") (Closed "bar\n")
        , both (Closed "") (Closed "]\n")
        , both (Line "quux") (Line "quux")
        ]

  describe "splitAbstractedTerm" $ do
    prop "preserves line count" $
      \ source -> let range = totalRange source in
        splitAbstractedTerm sequenceA (:<) (Identity source) (Identity (Info range mempty)) (Leaf source) `shouldBe` (Identity . lineMap (fmap (((:< Leaf source) . (`Info` mempty) &&& id))) <$> linesInRangeOfSource range source)

    let makeTerm = ((Free .) . Annotated) :: Info -> Syntax (Source Char) (SplitDiff (Source Char) Info) -> SplitDiff (Source Char) Info
    prop "outputs one row for single-line unchanged leaves" $
      forAll (arbitraryLeaf `suchThat` isOnSingleLine) $
        \ (source, info@(Info range categories), syntax) -> splitAbstractedTerm (zipDefaults mempty) makeTerm (pure source) (pure $ Info range categories) syntax `shouldBe` [
          both (pure (makeTerm info $ Leaf source, Range 0 (length source))) (pure (makeTerm info $ Leaf source, Range 0 (length source))) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` (\ a -> filter (/= '\n') (toList a) == toList a)) $
          \ source -> splitAbstractedTerm (zipDefaults mempty) makeTerm (pure source) (pure $ Info (totalRange source) mempty) (Indexed []) `shouldBe` [
            both (pure (makeTerm (Info (totalRange source) mempty) $ Indexed [], Range 0 (length source))) (pure (makeTerm (Info (totalRange source) mempty) $ Indexed [], Range 0 (length source))) ]

  describe "splitPatchByLines" $ do
    prop "starts at initial indices" $
      \ patch sources -> let indices = length <$> sources in
        fmap start . maybeFirst . Maybe.catMaybes <$> Both.unzip (fmap maybeFirst . fmap (fmap Prelude.snd) <$> splitPatchByLines ((Source.++) <$> sources <*> sources) (patchWithBoth patch (leafWithRangeInSource <$> sources <*> (Range <$> indices <*> ((2 *) <$> indices))))) `shouldBe` (<$) <$> indices <*> unPatch patch

    where
      isEmptyRow = and . fmap isEmpty

      isOnSingleLine (a, _, _) = filter (/= '\n') (toList a) == toList a

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info <$> pure (Range start $ start + 1) <*> mempty) (Leaf [ char ]) ], start + 1)

      leafWithRangesInSources sources ranges = Free $ Annotated (Info <$> ranges <*> pure mempty) (Leaf $ runBothWith (++) (toList <$> sources))

      leafWithRangeInSource source range = Info range mempty :< Leaf source

      patchWithBoth (Insert ()) = Insert . snd
      patchWithBoth (Delete ()) = Delete . fst
      patchWithBoth (Replace () ()) = runBothWith Replace
