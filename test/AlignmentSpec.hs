module AlignmentSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)
import Data.Text.Arbitrary ()

import Alignment
import ArbitraryTerm (arbitraryLeaf)
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Free hiding (unfold)
import Data.Adjoined
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
import Source hiding ((++), fromList)
import qualified Source
import SplitDiff
import Syntax

spec :: Spec
spec = parallel $ do
  describe "splitDiffByLines" $ do
    prop "preserves line counts in equal sources" $
      \ source ->
        length (splitDiffByLines (pure source) (Free $ Annotated (pure $ Info (totalRange source) mempty) (Indexed . Prelude.fst $ foldl combineIntoLeaves ([], 0) source))) `shouldBe` length (filter (== '\n') $ toString source) + 1

    prop "produces the maximum line count in inequal sources" $
      \ sources ->
        length (splitDiffByLines sources (Free $ Annotated ((`Info` mempty) . totalRange <$> sources) (Indexed $ leafWithRangesInSources sources <$> runBothWith (zipWith both) (actualLineRanges <$> (totalRange <$> sources) <*> sources)))) `shouldBe` runBothWith max ((+ 1) . length . filter (== '\n') . toString <$> sources)

  describe "splitAbstractedTerm" $ do
    prop "preserves line count" $
      \ source -> let range = totalRange source in
        splitAbstractedTerm (:<) (Identity source) (Identity (Info range mempty)) (Leaf source) `shouldBe` (Identity . lineMap (fmap (((:< Leaf source) . (`Info` mempty) &&& id))) <$> linesInRangeOfSource range source)

    let makeTerm = ((Free .) . Annotated) :: Info -> Syntax (Source Char) (SplitDiff (Source Char) Info) -> SplitDiff (Source Char) Info
    prop "outputs one row for single-line unchanged leaves" $
      forAll (arbitraryLeaf `suchThat` isOnSingleLine) $
        \ (source, info@(Info range categories), syntax) -> splitAbstractedTerm makeTerm (pure source) (pure $ Info range categories) syntax `shouldBe` fromList [
          both (pure (makeTerm info $ Leaf source, Range 0 (length source))) (pure (makeTerm info $ Leaf source, Range 0 (length source))) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` (\ a -> filter (/= '\n') (toString a) == toString a)) $
          \ source -> splitAbstractedTerm makeTerm (pure source) (pure $ Info (totalRange source) mempty) (Indexed []) `shouldBe` fromList [
            both (pure (makeTerm (Info (totalRange source) mempty) $ Indexed [], Range 0 (length source))) (pure (makeTerm (Info (totalRange source) mempty) $ Indexed [], Range 0 (length source))) ]

    where
      isOnSingleLine (a, _, _) = filter (/= '\n') (toString a) == toString a

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info <$> pure (Range start $ start + 1) <*> mempty) (Leaf [ char ]) ], start + 1)

      leafWithRangesInSources sources ranges = Free $ Annotated (Info <$> ranges <*> pure mempty) (Leaf $ runBothWith (++) (toString <$> sources))

      leafWithRangeInSource source range = Info range mempty :< Leaf source

      patchWithBoth (Insert ()) = Insert . snd
      patchWithBoth (Delete ()) = Delete . fst
      patchWithBoth (Replace () ()) = runBothWith Replace

      info :: Int -> Int -> Info
      info = ((`Info` mempty) .) . Range
