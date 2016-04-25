module AlignmentSpec where

import Test.Hspec
import Data.Text.Arbitrary ()

import Alignment
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Bifunctor.Join
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.List (intercalate)
import Data.These
import Diff
import Info
import Patch
import Prelude hiding (fst, snd)
import Range
import qualified Source
import SplitDiff
import Syntax

spec :: Spec
spec = parallel $ do
  describe "groupChildrenByLine" $ do
    it "produces symmetrical context" $
      groupChildrenByLine getRange (Join (These [Range 0 2, Range 2 4] [Range 0 2, Range 2 4])) ([] :: [Identity [Join These (SplitDiff String Info)]]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 2, []))
        , Join (These (Range 2 4, [])
                      (Range 2 4, []))
        ]

    it "produces asymmetrical context" $
      groupChildrenByLine getRange (Join (These [Range 0 2, Range 2 4] [Range 0 1])) ([] :: [Identity [Join These (SplitDiff String Info)]]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 1, []))
        , Join (This  (Range 2 4, []))
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

    it "aligns identical branches with multiple children on the same line" $
      alignDiff (pure (Source.fromList "[ foo, bar ]")) (pure (info 0 12) `branch` [ pure (info 2 5) `leaf` "foo", pure (info 7 10) `leaf` "bar" ]) `shouldBe`
        [ Join (runBothWith These (pure (info 0 12 `branch` [ info 2 5 `leaf` "foo", info 7 10 `leaf` "bar" ])) ) ]

    it "aligns insertions" $
      alignDiff (both (Source.fromList "a") (Source.fromList "a\nb")) (both (info 0 1) (info 0 3) `branch` [ pure (info 0 1) `leaf` "a", Pure (Insert (info 2 3 :< Leaf "b")) ]) `shouldBe`
        [ Join (These (info 0 1 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That (info 2 3 `branch` [ Pure (SplitInsert (info 2 3 :< Leaf "b")) ]))
        ]

    it "aligns context following insertions" $
      let sources = both (Source.fromList "a\nc") (Source.fromList "a\nb\nc")
          align = PrettyDiff sources . alignDiff sources in
      align (both (info 0 3) (info 0 5) `branch` [ pure (info 0 1) `leaf` "a", Pure (Insert (info 2 3 :< Leaf "b")), both (info 2 3) (info 4 5) `leaf` "c" ])
        `shouldBe` PrettyDiff sources
        [ Join (These (info 0 2 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That (info 2 4 `branch` [ Pure (SplitInsert (info 2 3 :< Leaf "b")) ]))
        , Join (These (info 2 3 `branch` [ info 2 3 `leaf` "c" ])
                      (info 4 5 `branch` [ info 4 5 `leaf` "c" ]))
        ]

branch :: annotation -> [Free (Annotated String annotation) patch] -> Free (Annotated String annotation) patch
branch annotation = Free . Annotated annotation . Indexed

leaf :: annotation -> String -> Free (Annotated String annotation) patch
leaf info = Free . Annotated info . Leaf

info :: Int -> Int -> Info
info = ((\ r -> Info r mempty 0) .) . Range

data PrettyDiff = PrettyDiff { unPrettySources :: Both (Source.Source Char), unPrettyLines :: [Join These (SplitDiff String Info)] }

instance Eq PrettyDiff where
  (==) = (==) `on` unPrettyLines

instance Show PrettyDiff where
  show (PrettyDiff sources lines) = intercalate "\n" (showLine 40 <$> lines)
    where showLine n line = case runJoin line of
            This before -> pad n (showDiff before (fst sources)) " | "
            That after -> showString (replicate n ' ') (showString " | " (showDiff after (snd sources)))
            These before after -> showDiff before (fst sources) ++ showString " | " (showDiff after (snd sources))
          showDiff diff = toList . stripNewlines . Source.slice (getRange diff)
          stripNewlines = fmap (\ c -> if c == '\n' then ' ' else c)
          pad n string = showString string . showString (replicate (max 0 (n - length string)) ' ')
