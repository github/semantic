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
import Data.Bifunctor.Join
import Data.Bifunctor.These
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

  describe "groupChildrenByLine" $ do
    it "produces symmetrical context" $
      groupChildrenByLine (Join (These [Range 0 2, Range 2 4] [Range 0 2, Range 2 4])) [] `shouldBe`
        [ Join (These (Range 0 2, [] :: [SplitDiff String Info])
                      (Range 0 2, []))
        , Join (These (Range 2 4, [])
                      (Range 2 4, []))
        ]

  describe "alignDiff" $ do
    it "aligns identical branches on a single line" $
      alignDiff (both (Source.fromList "[ foo ]") (Source.fromList "[ foo ]")) (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe`
        [ Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])) ]

    it "aligns identical branches spanning multiple lines" $
      alignDiff (both (Source.fromList "[\nfoo\n]") (Source.fromList "[\nfoo\n]")) (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe`
        [ Join (These (info 0 2 `branch` [])
                      (info 0 2 `branch` []))
        , Join (These (info 2 6 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (These (info 6 7 `branch` [])
                      (info 6 7 `branch` []))
        ]

    it "aligns reformatted branches" $
      alignDiff (both (Source.fromList "[ foo ]") (Source.fromList "[\nfoo\n]")) (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe`
        [ Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 0 2 `branch` []))
        , Join (That  (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (That  (info 6 7 `branch` []))
        ]

    it "aligns nodes following reformatted branches" $
      alignDiff (both (Source.fromList "[ foo ]\nbar\n") (Source.fromList "[\nfoo\n]\nbar\n")) (pure (info 0 12) `branch` [ pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ], pure (info 8 11) `leaf` "bar" ]) `shouldBe`
        [ Join (These (info 0 8 `branch` [ info 0 7 `branch` [ info 2 5 `leaf` "foo" ] ])
                      (info 0 2 `branch` [ info 0 2 `branch` [] ]))
        , Join (That  (info 2 6 `branch` [ info 2 6 `branch` [ info 2 5 `leaf` "foo" ] ]))
        , Join (That  (info 6 8 `branch` [ info 6 7 `branch` [] ]))
        , Join (These (info 8 12 `branch` [ info 8 11 `leaf` "bar" ])
                      (info 8 12 `branch` [ info 8 11 `leaf` "bar" ]))
        , Join (These (info 12 12 `branch` [])
                      (info 12 12 `branch` []))
        ]

    where
      isOnSingleLine (a, _, _) = filter (/= '\n') (toString a) == toString a

      combineIntoLeaves (leaves, start) char = (leaves ++ [ Free $ Annotated (Info <$> pure (Range start $ start + 1) <*> mempty) (Leaf [ char ]) ], start + 1)

      leafWithRangesInSources sources ranges = Free $ Annotated (Info <$> ranges <*> pure mempty) (Leaf $ runBothWith (++) (toString <$> sources))

      leafWithRangeInSource source range = Info range mempty :< Leaf source

      patchWithBoth (Insert ()) = Insert . snd
      patchWithBoth (Delete ()) = Delete . fst
      patchWithBoth (Replace () ()) = runBothWith Replace

      branch :: annotation -> [Free (Annotated String annotation) patch] -> Free (Annotated String annotation) patch
      branch annotation = Free . Annotated annotation . Indexed

      leaf :: annotation -> String -> Free (Annotated String annotation) patch
      leaf info = Free . Annotated info . Leaf

      info :: Int -> Int -> Info
      info = ((`Info` mempty) .) . Range
