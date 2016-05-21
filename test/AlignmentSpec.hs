{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module AlignmentSpec where

import Alignment
import ArbitraryTerm ()
import Control.Arrow ((&&&))
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
import Term
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "alignBranch" $ do
    it "produces symmetrical context" $
      alignBranch getRange ([] :: [Identity [Join These (SplitDiff String Info)]]) (both [Range 0 2, Range 2 4] [Range 0 2, Range 2 4]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 2, []))
        , Join (These (Range 2 4, [])
                      (Range 2 4, []))
        ]

    it "produces asymmetrical context" $
      alignBranch getRange ([] :: [Identity [Join These (SplitDiff String Info)]]) (both [Range 0 2, Range 2 4] [Range 0 1]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 1, []))
        , Join (This  (Range 2 4, []))
        ]

    prop "covers every input line" $
      pendingWith "TBD"

    prop "covers every input child" $
      pendingWith "TBD"

    prop "covers every line of every input child" $
      pendingWith "TBD"

  describe "alignDiff" $ do
    it "aligns identical branches on a single line" $
      let sources = both (Source.fromList "[ foo ]") (Source.fromList "[ foo ]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])) ]

    it "aligns identical branches spanning multiple lines" $
      let sources = both (Source.fromList "[\nfoo\n]") (Source.fromList "[\nfoo\n]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 2 `branch` [])
                      (info 0 2 `branch` []))
        , Join (These (info 2 6 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (These (info 6 7 `branch` [])
                      (info 6 7 `branch` []))
        ]

    it "aligns reformatted branches" $
      let sources = both (Source.fromList "[ foo ]") (Source.fromList "[\nfoo\n]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` prettyDiff sources
        [ Join (That  (info 0 2 `branch` []))
        , Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (That  (info 6 7 `branch` []))
        ]

    it "aligns nodes following reformatted branches" $
      let sources = both (Source.fromList "[ foo ]\nbar\n") (Source.fromList "[\nfoo\n]\nbar\n") in
      align sources (pure (info 0 12) `branch` [ pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ], pure (info 8 11) `leaf` "bar" ]) `shouldBe` prettyDiff sources
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
      align sources (pure (info 0 12) `branch` [ pure (info 2 5) `leaf` "foo", pure (info 7 10) `leaf` "bar" ]) `shouldBe` prettyDiff sources
        [ Join (runBothWith These (pure (info 0 12 `branch` [ info 2 5 `leaf` "foo", info 7 10 `leaf` "bar" ])) ) ]

    it "aligns insertions" $
      let sources = both (Source.fromList "a") (Source.fromList "a\nb") in
      align sources (both (info 0 1) (info 0 3) `branch` [ pure (info 0 1) `leaf` "a", insert (info 2 3 `leaf` "b") ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 1 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That  (info 2 3 `branch` [ insert (info 2 3 `leaf` "b") ]))
        ]

    it "aligns total insertions" $
      let sources = both (Source.fromList "") (Source.fromList "a") in
      align sources (insert (info 0 1 `leaf` "a")) `shouldBe` prettyDiff sources
        [ Join (That (insert (info 0 1 `leaf` "a"))) ]

    it "aligns insertions into empty branches" $
      let sources = both (Source.fromList "[ ]") (Source.fromList "[a]") in
      align sources (pure (info 0 3) `branch` [ insert (info 1 2 `leaf` "a") ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 3 `branch` [])
                      (info 0 3 `branch` [ insert (info 1 2 `leaf` "a") ])) ]

    it "aligns symmetrically following insertions" $
      let sources = both (Source.fromList "a\nc") (Source.fromList "a\nb\nc") in
      align sources (both (info 0 3) (info 0 5) `branch` [ pure (info 0 1) `leaf` "a", insert (info 2 3 `leaf` "b"), both (info 2 3) (info 4 5) `leaf` "c" ])
        `shouldBe` prettyDiff sources
        [ Join (These (info 0 2 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That  (info 2 4 `branch` [ insert (info 2 3 `leaf` "b") ]))
        , Join (These (info 2 3 `branch` [ info 2 3 `leaf` "c" ])
                      (info 4 5 `branch` [ info 4 5 `leaf` "c" ]))
        ]

    it "symmetrical nodes force the alignment of asymmetrical nodes on both sides" $
      let sources = both (Source.fromList "[ a, b ]") (Source.fromList "[ b, c ]") in
      align sources (pure (info 0 8) `branch` [ delete (info 2 3 `leaf` "a"), both (info 5 6) (info 2 3) `leaf` "b", insert (info 5 6 `leaf` "c") ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 8 `branch` [ delete (info 2 3 `leaf` "a"), info 5 6 `leaf` "b" ])
                      (info 0 8 `branch` [ info 2 3 `leaf` "b", insert (info 5 6 `leaf` "c") ])) ]

    it "when one of two symmetrical nodes must be split, splits the latter" $
      let sources = both (Source.fromList "[ a, b ]") (Source.fromList "[ a\n, b\n]") in
      align sources (both (info 0 8) (info 0 9) `branch` [ pure (info 2 3) `leaf` "a", both (info 5 6) (info 6 7) `leaf` "b" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 8 `branch` [ info 2 3 `leaf` "a", info 5 6 `leaf` "b" ])
                      (info 0 4 `branch` [ info 2 3 `leaf` "a" ]))
        , Join (That  (info 4 8 `branch` [ info 6 7 `leaf` "b" ]))
        , Join (That  (info 8 9 `branch` []))
        ]

    it "aligns deletions before insertions" $
      let sources = both (Source.fromList "[ a ]") (Source.fromList "[ b ]") in
      align sources (pure (info 0 5) `branch` [ delete (info 2 3 `leaf` "a"), insert (info 2 3 `leaf` "b") ]) `shouldBe` prettyDiff sources
        [ Join (This (info 0 5 `branch` [ delete (info 2 3 `leaf` "a") ]))
        , Join (That (info 0 5 `branch` [ insert (info 2 3 `leaf` "b") ]))
        ]

    it "aligns context-only lines symmetrically" $
      let sources = both (Source.fromList "[\n  a\n,\n  b\n]") (Source.fromList "[\n  a, b\n\n\n]") in
      align sources (both (info 0 13) (info 0 12) `branch` [ pure (info 4 5) `leaf` "a", both (info 10 11) (info 7 8) `leaf` "b" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 2 `branch` [])
                      (info 0 2 `branch` []))
        , Join (These (info 2 6 `branch` [ info 4 5 `leaf` "a" ])
                      (info 2 9 `branch` [ info 4 5 `leaf` "a", info 7 8 `leaf` "b" ]))
        , Join (These (info 6 8 `branch` [])
                      (info 9 10 `branch` []))
        , Join (These (info 8 12 `branch` [ info 10 11 `leaf` "b" ])
                      (info 10 11 `branch` []))
        , Join (These (info 12 13 `branch` [])
                      (info 11 12 `branch` []))
        ]

    it "aligns asymmetrical nodes preceding their symmetrical siblings conservatively" $
      let sources = both (Source.fromList "[ b, c ]") (Source.fromList "[ a\n, c\n]") in
      align sources (both (info 0 8) (info 0 9) `branch` [ insert (info 2 3 `leaf` "a"), delete (info 2 3 `leaf` "b"), both (info 5 6) (info 6 7) `leaf` "c" ]) `shouldBe` prettyDiff sources
        [ Join (That  (info 0 4 `branch` [ insert (info 2 3 `leaf` "a") ]))
        , Join (These (info 0 8 `branch` [ delete (info 2 3 `leaf` "b"), info 5 6 `leaf` "c" ])
                      (info 4 8 `branch` [ info 6 7 `leaf` "c" ]))
        , Join (That  (info 8 9 `branch` []))
        ]

  describe "numberedRows" $
    prop "counts only non-empty values" $
      \ xs -> counts (numberedRows (xs :: [Join These Char])) `shouldBe` length . catMaybes <$> Join (unalign (runJoin <$> xs))

data Child = Child
  { childKey :: String
  , childContents :: String
  , childMargin :: String
  }

data BranchElement
  = Child' String (Join These String {- newlines or asterisks -})
  | Margin (Join These String {- newlines or hyphens -})

toSource :: [BranchElement] -> Source.Source Char
toSource = Source.fromList . concatMap show

toSources :: [Join These BranchElement] -> Both (Source.Source Char)
toSources = fmap toSource . toChildLists

toChildLists :: [Join These BranchElement] -> Both [BranchElement]
toChildLists = foldMap (modifyJoin (fromThese [] []) . fmap (:[]))

instance Arbitrary Child where
  arbitrary = Child <$> key <*> contents <*> margin
    where key = listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
          contents = listOf (padding '*')
          margin = listOf (padding '-')
          padding char = frequency [ (10, pure char)
                                   , (1, pure '\n') ]

  shrink Child {..} | null childContents, null childMargin = []
                    | otherwise = Child childKey <$> "" : shrinkList (const []) childContents <*> "" : shrinkList (const []) childMargin

instance Arbitrary BranchElement where
  arbitrary = oneof [ Child' <$> key <*> joinTheseOf contents
                    , Margin <$> joinTheseOf margin ]
    where key = listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
          contents = listOf (padding '*')
          margin = listOf (padding '-')
          padding char = frequency [ (10, pure char)
                                   , (1, pure '\n') ]
          joinTheseOf g = oneof [ Join . This <$> g
                                , Join . That <$> g
                                , (Join .) . These <$> g <*> g ]

  shrink (Child' key contents) = Child' key <$> traverse (shrinkList (const [])) contents
  shrink (Margin contents) = Margin <$> traverse (shrinkList (const [])) contents

instance Show Child where
  show Child {..} = childMargin ++ "(" ++ childKey ++ childContents ++ ")"

instance Show BranchElement where
  show (Child' key contents) = showThese (showContents <$> contents)
    where showContents contents = "(" ++ key ++ contents ++ ")"
  show (Margin contents) = showThese contents

showThese :: Join These String -> String
showThese = these ('<':) ('>':) (\ a b -> a ++ " | " ++ b) . runJoin

counts :: [Join These (Int, a)] -> Both Int
counts numbered = fromMaybe 0 . getLast . mconcat . fmap Last <$> Join (unalign (runJoin . fmap Prelude.fst <$> numbered))

align :: Both (Source.Source Char) -> ConstructibleFree (Patch (Term String Info)) (Both Info) -> PrettyDiff (SplitDiff String Info)
align sources = PrettyDiff sources . fmap (fmap (getRange &&& id)) . alignDiff sources . deconstruct

info :: Int -> Int -> Info
info = ((\ r -> Info r mempty 0) .) . Range

prettyDiff :: Both (Source.Source Char) -> [Join These (ConstructibleFree (SplitPatch (Term String Info)) Info)] -> PrettyDiff (SplitDiff String Info)
prettyDiff sources = PrettyDiff sources . fmap (fmap ((getRange &&& id) . deconstruct))

data PrettyDiff a = PrettyDiff { unPrettySources :: Both (Source.Source Char), unPrettyLines :: [Join These (Range, a)] }
  deriving Eq

instance Show a => Show (PrettyDiff a) where
  show (PrettyDiff sources lines) = prettyPrinted -- ++ "\n" ++ show lines
    where prettyPrinted = showLine (maximum (0 : (maximum . fmap length <$> shownLines))) <$> shownLines >>= ('\n':)
          shownLines = catMaybes $ toBoth <$> lines
          showLine n line = uncurry ((++) . (++ " | ")) (fromThese (replicate n ' ') (replicate n ' ') (runJoin (pad n <$> line)))
          showDiff (range, _) = filter (/= '\n') . toList . Source.slice range
          pad n string = showString (take n string) (replicate (max 0 (n - length string)) ' ')
          toBoth them = showDiff <$> them `applyThese` modifyJoin (uncurry These) sources

newtype ConstructibleFree patch annotation = ConstructibleFree { deconstruct :: Free (Annotated String annotation) patch }


class PatchConstructible p where
  insert :: Term String Info -> p
  delete :: Term String Info -> p

instance PatchConstructible (Patch (Term String Info)) where
  insert = Insert
  delete = Delete

instance PatchConstructible (SplitPatch (Term String Info)) where
  insert = SplitInsert
  delete = SplitDelete

instance PatchConstructible patch => PatchConstructible (ConstructibleFree patch annotation) where
  insert = ConstructibleFree . Pure . insert
  delete = ConstructibleFree . Pure . delete


class SyntaxConstructible s where
  leaf :: annotation -> String -> s annotation
  branch :: annotation -> [s annotation] -> s annotation

instance SyntaxConstructible (ConstructibleFree patch) where
  leaf info = ConstructibleFree . Free . Annotated info . Leaf
  branch info = ConstructibleFree . Free . Annotated info . Indexed . fmap deconstruct

instance SyntaxConstructible (Cofree (Syntax String)) where
  info `leaf` value = info :< Leaf value
  info `branch` children = info :< Indexed children
