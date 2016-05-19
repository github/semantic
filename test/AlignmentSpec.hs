module AlignmentSpec where

import Alignment
import ArbitraryTerm ()
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Align hiding (align)
import Data.Bifunctor.Join
import Data.Foldable (toList)
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Text.Arbitrary ()
import Data.These
import Diff
import Info
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import qualified Source
import SplitDiff
import Syntax
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "alignChildrenInRanges" $ do
    it "produces symmetrical context" $
      alignChildrenInRanges getRange (Join (These [Range 0 2, Range 2 4] [Range 0 2, Range 2 4])) ([] :: [Identity [Join These (SplitDiff String Info)]]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 2, []))
        , Join (These (Range 2 4, [])
                      (Range 2 4, []))
        ]

    it "produces asymmetrical context" $
      alignChildrenInRanges getRange (Join (These [Range 0 2, Range 2 4] [Range 0 1])) ([] :: [Identity [Join These (SplitDiff String Info)]]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 1, []))
        , Join (This  (Range 2 4, []))
        ]

  describe "alignDiff" $ do
    it "aligns identical branches on a single line" $
      let sources = both (Source.fromList "[ foo ]") (Source.fromList "[ foo ]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` PrettyDiff sources
        [ Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])) ]

    it "aligns identical branches spanning multiple lines" $
      let sources = both (Source.fromList "[\nfoo\n]") (Source.fromList "[\nfoo\n]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` PrettyDiff sources
        [ Join (These (info 0 2 `branch` [])
                      (info 0 2 `branch` []))
        , Join (These (info 2 6 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (These (info 6 7 `branch` [])
                      (info 6 7 `branch` []))
        ]

    it "aligns reformatted branches" $
      let sources = both (Source.fromList "[ foo ]") (Source.fromList "[\nfoo\n]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` PrettyDiff sources
        [ Join (That  (info 0 2 `branch` []))
        , Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (That  (info 6 7 `branch` []))
        ]

    it "aligns nodes following reformatted branches" $
      let sources = both (Source.fromList "[ foo ]\nbar\n") (Source.fromList "[\nfoo\n]\nbar\n") in
      align sources (pure (info 0 12) `branch` [ pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ], pure (info 8 11) `leaf` "bar" ]) `shouldBe` PrettyDiff sources
        [ Join (That  (info 0 2 `branch` [ info 0 2 `branch` [] ]))
        , Join (These (info 0 8 `branch` [ info 0 7 `branch` [ info 2 5 `leaf` "foo" ] ])
                      (info 2 6 `branch` [ info 2 6 `branch` [ info 2 5 `leaf` "foo" ] ]))
        , Join (That  (info 6 8 `branch` [ info 6 7 `branch` [] ]))
        , Join (These (info 8 12 `branch` [ info 8 11 `leaf` "bar" ])
                      (info 8 12 `branch` [ info 8 11 `leaf` "bar" ]))
        , Join (These (info 12 12 `branch` [])
                      (info 12 12 `branch` []))
        ]

    it "aligns identical branches with multiple children on the same line" $
      let sources = pure (Source.fromList "[ foo, bar ]") in
      align sources (pure (info 0 12) `branch` [ pure (info 2 5) `leaf` "foo", pure (info 7 10) `leaf` "bar" ]) `shouldBe` PrettyDiff sources
        [ Join (runBothWith These (pure (info 0 12 `branch` [ info 2 5 `leaf` "foo", info 7 10 `leaf` "bar" ])) ) ]

    it "aligns insertions" $
      let sources = both (Source.fromList "a") (Source.fromList "a\nb") in
      align sources (both (info 0 1) (info 0 3) `branch` [ pure (info 0 1) `leaf` "a", Pure (Insert (info 2 3 :< Leaf "b")) ]) `shouldBe` PrettyDiff sources
        [ Join (These (info 0 1 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That  (info 2 3 `branch` [ Pure (SplitInsert (info 2 3 :< Leaf "b")) ]))
        ]

    it "aligns total insertions" $
      let sources = both (Source.fromList "") (Source.fromList "a") in
      align sources (Pure (Insert (info 0 1 :< Leaf "a"))) `shouldBe` PrettyDiff sources
        [ Join (That (Pure (SplitInsert (info 0 1 :< Leaf "a")))) ]

    it "aligns insertions into empty branches" $
      let sources = both (Source.fromList "[ ]") (Source.fromList "[a]") in
      align sources (pure (info 0 3) `branch` [ Pure (Insert (info 1 2 :< Leaf "a")) ]) `shouldBe` PrettyDiff sources
        [ Join (These (info 0 3 `branch` [])
                      (info 0 3 `branch` [ Pure (SplitInsert (info 1 2 :< Leaf "a")) ])) ]

    it "aligns symmetrically following insertions" $
      let sources = both (Source.fromList "a\nc") (Source.fromList "a\nb\nc") in
      align sources (both (info 0 3) (info 0 5) `branch` [ pure (info 0 1) `leaf` "a", Pure (Insert (info 2 3 :< Leaf "b")), both (info 2 3) (info 4 5) `leaf` "c" ])
        `shouldBe` PrettyDiff sources
        [ Join (These (info 0 2 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That  (info 2 4 `branch` [ Pure (SplitInsert (info 2 3 :< Leaf "b")) ]))
        , Join (These (info 2 3 `branch` [ info 2 3 `leaf` "c" ])
                      (info 4 5 `branch` [ info 4 5 `leaf` "c" ]))
        ]

    it "symmetrical nodes force the alignment of preceding coincident asymmetrical nodes" $
      let sources = both (Source.fromList "[ a, b ]") (Source.fromList "[ b ]") in
      align sources (both (info 0 8) (info 0 5) `branch` [ Pure (Delete (info 2 3 :< Leaf "a")), both (info 5 6) (info 2 3) `leaf` "b" ]) `shouldBe` PrettyDiff sources
        [ Join (These (info 0 8 `branch` [ Pure (SplitDelete (info 2 3 :< Leaf "a")), info 5 6 `leaf` "b" ])
                      (info 0 5 `branch` [ info 2 3 `leaf` "b" ])) ]

    it "symmetrical nodes force the alignment of following coincident asymmetrical nodes" $
      let sources = both (Source.fromList "[ a, b ]") (Source.fromList "[ a ]") in
      align sources (both (info 0 8) (info 0 5) `branch` [ both (info 5 6) (info 2 3) `leaf` "a", Pure (Delete (info 2 3 :< Leaf "b")) ]) `shouldBe` PrettyDiff sources
        [ Join (These (info 0 8 `branch` [ info 5 6 `leaf` "a", Pure (SplitDelete (info 2 3 :< Leaf "b")) ])
                      (info 0 5 `branch` [ info 2 3 `leaf` "a" ])) ]

  describe "numberedRows" $
    prop "counts only non-empty values" $
      \ xs -> counts (numberedRows (xs :: [Join These Char])) `shouldBe` length . catMaybes <$> Join (unalign (runJoin <$> xs))

counts :: [Join These (Int, a)] -> Both Int
counts numbered = fromMaybe 0 . getLast . mconcat . fmap Last <$> Join (unalign (runJoin . fmap Prelude.fst <$> numbered))

align :: Both (Source.Source Char) -> Diff String Info -> PrettyDiff
align sources = PrettyDiff sources . alignDiff sources

branch :: annotation -> [Free (Annotated String annotation) patch] -> Free (Annotated String annotation) patch
branch annotation = Free . Annotated annotation . Indexed

leaf :: annotation -> String -> Free (Annotated String annotation) patch
leaf info = Free . Annotated info . Leaf

info :: Int -> Int -> Info
info = ((\ r -> Info r mempty 0) .) . Range

data PrettyDiff = PrettyDiff { unPrettySources :: Both (Source.Source Char), unPrettyLines :: [Join These (SplitDiff String Info)] }
  deriving Eq

instance Show PrettyDiff where
  show (PrettyDiff sources lines) = showLine (maximum (0 : (maximum . fmap length <$> shownLines))) <$> shownLines >>= ('\n':)
    where shownLines = catMaybes $ toBoth <$> lines
          showLine n line = uncurry ((++) . (++ " | ")) (fromThese (replicate n ' ') (replicate n ' ') (runJoin (pad n <$> line)))
          showDiff diff = filter (/= '\n') . toList . Source.slice (getRange diff)
          pad n string = showString (take n string) (replicate (max 0 (n - length string)) ' ')
          toBoth them = showDiff <$> them `applyThese` modifyJoin (uncurry These) sources
