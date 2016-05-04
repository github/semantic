module AlignmentSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)
import Data.Text.Arbitrary ()

import Alignment
import ArbitraryTerm (arbitraryLeaf)
import Control.Arrow
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free hiding (unfold)
import Data.Adjoined
import Data.Copointed
import Data.Functor.Both as Both
import Diff
import Info
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
        length (splitDiffByLines (pure source) (free . Free $ (pure $ Info (totalRange source) mempty 1) :< (Indexed . Prelude.fst $ foldl combineIntoLeaves ([], 0) source))) `shouldBe` length (filter (== '\n') $ toString source) + 1

    prop "produces the maximum line count in inequal sources" $
      \ sources -> let ranges = actualLineRanges <$> (totalRange <$> sources) <*> sources in
        length (splitDiffByLines sources (free . Free $ ((\ s -> Info (totalRange s) mempty 0) <$> sources) :< (Indexed $ leafWithRangesInSources sources <$> runBothWith (zipWith both) ranges))) `shouldBe` runBothWith max ((+ 1) . length . filter (== '\n') . toString <$> sources)

  describe "splitAbstractedTerm" $ do
    prop "preserves line count" $
      \ source -> let range = totalRange source in
        splitAbstractedTerm ((cofree .) . (:<)) (Identity source) (Identity (Info range mempty 0) :< Leaf source) `shouldBe` (Identity . lineMap (fmap (cofree . (:< Leaf source) . (\ r -> Info r mempty 0) &&& id)) <$> linesInRangeOfSource range source)

    let makeTerm = ((free .) . (Free .) . (:<)) :: Info -> Syntax (Source Char) (SplitDiff (Source Char) Info) -> SplitDiff (Source Char) Info
    prop "outputs one row for single-line unchanged leaves" $
      forAll (arbitraryLeaf `suchThat` isOnSingleLine) $
        \ (source, (Info range categories _), syntax) -> splitAbstractedTerm makeTerm (pure source) (pure (Info range categories 0) :< syntax) `shouldBe` fromList [
          both (pure (makeTerm (Info range categories 0) $ Leaf source, Range 0 (length source))) (pure (makeTerm (Info range categories 0) $ Leaf source, Range 0 (length source))) ]

    prop "outputs one row for single-line empty unchanged indexed nodes" $
      forAll (arbitrary `suchThat` (\ a -> filter (/= '\n') (toString a) == toString a)) $
          \ source -> splitAbstractedTerm makeTerm (pure source) (pure (Info (totalRange source) mempty 0) :< Indexed []) `shouldBe` fromList [
            both (pure (makeTerm (Info (totalRange source) mempty 0) $ Indexed [], Range 0 (length source))) (pure (makeTerm (Info (totalRange source) mempty 0) $ Indexed [], Range 0 (length source))) ]

    where
      isOnSingleLine (a, _, _) = filter (/= '\n') (toString a) == toString a

      combineIntoLeaves (leaves, start) char = (leaves ++ [ free . Free $ (Info <$> pure (Range start $ start + 1) <*> mempty <*> pure 1) :< Leaf [ char ] ], start + 1)

      leafWithRangesInSources sources ranges = free . Free $ (Info <$> ranges <*> pure mempty <*> pure 1) :< (Leaf $ runBothWith (++) (toString <$> sources))

      leafWithRangeInSource source range = Info range mempty 1 :< Leaf source

      patchWithBoth (Insert ()) = Insert . snd
      patchWithBoth (Delete ()) = Delete . fst
      patchWithBoth (Replace () ()) = runBothWith Replace
