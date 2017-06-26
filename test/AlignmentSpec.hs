{-# LANGUAGE DataKinds #-}
module AlignmentSpec where

import Alignment
import Control.Monad.State
import Data.Align hiding (align)
import Data.Bifunctor
import Data.Bifunctor.Join
import Data.Functor.Both as Both
import Data.Functor.Listable
import Data.List (nub)
import Data.Monoid hiding ((<>))
import Data.Range
import Data.Record
import qualified Data.Source as Source
import qualified Data.Text as Text
import Data.These
import Patch
import Prologue hiding (fst, snd)
import qualified Prologue
import SplitDiff
import Syntax
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck
import GHC.Show (Show(..))

spec :: Spec
spec = parallel $ do
  describe "alignBranch" $ do
    it "produces symmetrical context" $
      alignBranch getRange ([] :: [Join These (SplitDiff (Syntax Text) (Record '[Range]))]) (both [Range 0 2, Range 2 4] [Range 0 2, Range 2 4]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 2, []))
        , Join (These (Range 2 4, [])
                      (Range 2 4, []))
        ]

    it "produces asymmetrical context" $
      alignBranch getRange ([] :: [Join These (SplitDiff (Syntax Text) (Record '[Range]))]) (both [Range 0 2, Range 2 4] [Range 0 1]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 1, []))
        , Join (This  (Range 2 4, []))
        ]

    prop "covers every input line" $
      \ elements -> let (_, children, ranges) = toAlignBranchInputs elements in
        join <$> traverse (modifyJoin (fromThese [] []) . fmap (pure . Prologue.fst)) (alignBranch Prologue.snd children ranges) `shouldBe` ranges

    prop "covers every input child" $
      \ elements -> let (_, children, ranges) = toAlignBranchInputs elements in
        sort (nub (keysOfAlignedChildren (alignBranch Prologue.snd children ranges))) `shouldBe` sort (nub (catMaybes (branchElementKey <$> elements)))

    prop "covers every line of every input child" $
      \ elements -> let (_, children, ranges) = toAlignBranchInputs elements in
        sort (keysOfAlignedChildren (alignBranch Prologue.snd children ranges)) `shouldBe` sort (do
          line <- children
          these (pure . Prologue.fst) (pure . Prologue.fst) (\ (k1, _) (k2, _) -> [ k1, k2 ]) . runJoin $ line)

  describe "alignDiff" $ do
    it "aligns identical branches on a single line" $
      let sources = both (Source.fromText "[ foo ]") (Source.fromText "[ foo ]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])) ]

    it "aligns identical branches spanning multiple lines" $
      let sources = both (Source.fromText "[\nfoo\n]") (Source.fromText "[\nfoo\n]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 2 `branch` [])
                      (info 0 2 `branch` []))
        , Join (These (info 2 6 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (These (info 6 7 `branch` [])
                      (info 6 7 `branch` []))
        ]

    it "aligns reformatted branches" $
      let sources = both (Source.fromText "[ foo ]") (Source.fromText "[\nfoo\n]") in
      align sources (pure (info 0 7) `branch` [ pure (info 2 5) `leaf` "foo" ]) `shouldBe` prettyDiff sources
        [ Join (That  (info 0 2 `branch` []))
        , Join (These (info 0 7 `branch` [ info 2 5 `leaf` "foo" ])
                      (info 2 6 `branch` [ info 2 5 `leaf` "foo" ]))
        , Join (That  (info 6 7 `branch` []))
        ]

    it "aligns nodes following reformatted branches" $
      let sources = both (Source.fromText "[ foo ]\nbar\n") (Source.fromText "[\nfoo\n]\nbar\n") in
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
      let sources = pure (Source.fromText "[ foo, bar ]") in
      align sources (pure (info 0 12) `branch` [ pure (info 2 5) `leaf` "foo", pure (info 7 10) `leaf` "bar" ]) `shouldBe` prettyDiff sources
        [ Join (runBothWith These (pure (info 0 12 `branch` [ info 2 5 `leaf` "foo", info 7 10 `leaf` "bar" ])) ) ]

    it "aligns insertions" $
      let sources = both (Source.fromText "a") (Source.fromText "a\nb") in
      align sources (both (info 0 1) (info 0 3) `branch` [ pure (info 0 1) `leaf` "a", insert (info 2 3 `leaf` "b") ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 1 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That  (info 2 3 `branch` [ insert (info 2 3 `leaf` "b") ]))
        ]

    it "aligns total insertions" $
      let sources = both (Source.fromText "") (Source.fromText "a") in
      align sources (insert (info 0 1 `leaf` "a")) `shouldBe` prettyDiff sources
        [ Join (That (insert (info 0 1 `leaf` "a"))) ]

    it "aligns insertions into empty branches" $
      let sources = both (Source.fromText "[ ]") (Source.fromText "[a]") in
      align sources (pure (info 0 3) `branch` [ insert (info 1 2 `leaf` "a") ]) `shouldBe` prettyDiff sources
        [ Join (That  (info 0 3 `branch` [ insert (info 1 2 `leaf` "a") ]))
        , Join (This  (info 0 3 `branch` []))
        ]

    it "aligns symmetrically following insertions" $
      let sources = both (Source.fromText "a\nc") (Source.fromText "a\nb\nc") in
      align sources (both (info 0 3) (info 0 5) `branch` [ pure (info 0 1) `leaf` "a", insert (info 2 3 `leaf` "b"), both (info 2 3) (info 4 5) `leaf` "c" ])
        `shouldBe` prettyDiff sources
        [ Join (These (info 0 2 `branch` [ info 0 1 `leaf` "a" ])
                      (info 0 2 `branch` [ info 0 1 `leaf` "a" ]))
        , Join (That  (info 2 4 `branch` [ insert (info 2 3 `leaf` "b") ]))
        , Join (These (info 2 3 `branch` [ info 2 3 `leaf` "c" ])
                      (info 4 5 `branch` [ info 4 5 `leaf` "c" ]))
        ]

    it "symmetrical nodes force the alignment of asymmetrical nodes on both sides" $
      let sources = both (Source.fromText "[ a, b ]") (Source.fromText "[ b, c ]") in
      align sources (pure (info 0 8) `branch` [ delete (info 2 3 `leaf` "a"), both (info 5 6) (info 2 3) `leaf` "b", insert (info 5 6 `leaf` "c") ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 8 `branch` [ delete (info 2 3 `leaf` "a"), info 5 6 `leaf` "b" ])
                      (info 0 8 `branch` [ info 2 3 `leaf` "b", insert (info 5 6 `leaf` "c") ])) ]

    it "when one of two symmetrical nodes must be split, splits the latter" $
      let sources = both (Source.fromText "[ a, b ]") (Source.fromText "[ a\n, b\n]") in
      align sources (both (info 0 8) (info 0 9) `branch` [ pure (info 2 3) `leaf` "a", both (info 5 6) (info 6 7) `leaf` "b" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 8 `branch` [ info 2 3 `leaf` "a", info 5 6 `leaf` "b" ])
                      (info 0 4 `branch` [ info 2 3 `leaf` "a" ]))
        , Join (That  (info 4 8 `branch` [ info 6 7 `leaf` "b" ]))
        , Join (That  (info 8 9 `branch` []))
        ]

    it "aligns deletions before insertions" $
      let sources = both (Source.fromText "[ a ]") (Source.fromText "[ b ]") in
      align sources (pure (info 0 5) `branch` [ delete (info 2 3 `leaf` "a"), insert (info 2 3 `leaf` "b") ]) `shouldBe` prettyDiff sources
        [ Join (This (info 0 5 `branch` [ delete (info 2 3 `leaf` "a") ]))
        , Join (That (info 0 5 `branch` [ insert (info 2 3 `leaf` "b") ]))
        ]

    it "aligns context-only lines symmetrically" $
      let sources = both (Source.fromText "[\n  a\n,\n  b\n]") (Source.fromText "[\n  a, b\n\n\n]") in
      align sources (both (info 0 13) (info 0 12) `branch` [ pure (info 4 5) `leaf` "a", both (info 10 11) (info 7 8) `leaf` "b" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 2 `branch` [])
                      (info 0 2 `branch` []))
        , Join (These (info 2 6 `branch` [ info 4 5 `leaf` "a" ])
                      (info 2 9 `branch` [ info 4 5 `leaf` "a", info 7 8 `leaf` "b" ]))
        , Join (These (info 6 8 `branch` [])
                      (info 9 10 `branch` []))
        , Join (This  (info 8 12 `branch` [ info 10 11 `leaf` "b" ]))
        , Join (These (info 12 13 `branch` [])
                      (info 10 11 `branch` []))
        , Join (That  (info 11 12 `branch` []))
        ]

    it "aligns asymmetrical nodes preceding their symmetrical siblings conservatively" $
      let sources = both (Source.fromText "[ b, c ]") (Source.fromText "[ a\n, c\n]") in
      align sources (both (info 0 8) (info 0 9) `branch` [ insert (info 2 3 `leaf` "a"), delete (info 2 3 `leaf` "b"), both (info 5 6) (info 6 7) `leaf` "c" ]) `shouldBe` prettyDiff sources
        [ Join (That  (info 0 4 `branch` [ insert (info 2 3 `leaf` "a") ]))
        , Join (These (info 0 8 `branch` [ delete (info 2 3 `leaf` "b"), info 5 6 `leaf` "c" ])
                      (info 4 8 `branch` [ info 6 7 `leaf` "c" ]))
        , Join (That  (info 8 9 `branch` []))
        ]

    it "aligns symmetrical reformatted nodes" $
      let sources = both (Source.fromText "a [ b ]\nc") (Source.fromText "a [\nb\n]\nc") in
      align sources (pure (info 0 9) `branch` [ pure (info 0 1) `leaf` "a", pure (info 2 7) `branch` [ pure (info 4 5) `leaf` "b" ], pure (info 8 9) `leaf` "c" ]) `shouldBe` prettyDiff sources
        [ Join (These (info 0 8 `branch` [ info 0 1 `leaf` "a", info 2 7 `branch` [ info 4 5 `leaf` "b" ] ])
                      (info 0 4 `branch` [ info 0 1 `leaf` "a", info 2 4 `branch` [] ]))
        , Join (That  (info 4 6 `branch` [ info 4 6 `branch` [ info 4 5 `leaf` "b" ] ]))
        , Join (That  (info 6 8 `branch` [ info 6 7 `branch` [] ]))
        , Join (These (info 8 9 `branch` [ info 8 9 `leaf` "c" ])
                      (info 8 9 `branch` [ info 8 9 `leaf` "c" ]))
        ]

  describe "numberedRows" $ do
    prop "counts only non-empty values" $
      \ xs -> counts (numberedRows (unListableF <$> xs :: [Join These Char])) `shouldBe` length . catMaybes <$> Join (unalign (runJoin . unListableF <$> xs))

data BranchElement
  = Child Text (Join These Text)
  | Margin (Join These Text)
  deriving Show

branchElementKey :: BranchElement -> Maybe Text
branchElementKey (Child key _) = Just key
branchElementKey _ = Nothing

toAlignBranchInputs :: [BranchElement] -> (Both Source.Source, [Join These (Text, Range)], Both [Range])
toAlignBranchInputs elements = (sources, join . (`evalState` both 0 0) . traverse go $ elements, ranges)
  where go :: BranchElement -> State (Both Int) [Join These (Text, Range)]
        go child@(Child key _) = do
          lines <- traverse (\ (Child _ contents) -> do
            prev <- get
            let next = (+) <$> prev <*> modifyJoin (fromThese 0 0) (Text.length <$> contents)
            put next
            pure $! modifyJoin (runBothWith bimap (const <$> (Range <$> prev <*> next))) contents) (alignBranchElement child)
          pure $! fmap ((,) key) <$> lines
        go (Margin contents) = do
          prev <- get
          put $ (+) <$> prev <*> modifyJoin (fromThese 0 0) (Text.length <$> contents)
          pure []
        alignBranchElement element = case element of
          Child key contents -> Child key <$> joinCrosswalk lines contents
          Margin contents -> Margin <$> joinCrosswalk lines contents
          where lines = fmap Source.toText . Source.sourceLines . Source.fromText
        sources = foldMap Source.fromText <$> bothContents elements
        ranges = fmap (filter (\ (Range start end) -> start /= end)) $ Source.sourceLineRangesWithin <$> (Source.totalRange <$> sources) <*> sources
        bothContents = foldMap (modifyJoin (fromThese [] []) . fmap (:[]) . branchElementContents)
        branchElementContents (Child _ contents) = contents
        branchElementContents (Margin contents) = contents

keysOfAlignedChildren :: [Join These (Range, [(Text, Range)])] -> [Text]
keysOfAlignedChildren lines = lines >>= these identity identity (<>) . runJoin . fmap (fmap Prologue.fst . Prologue.snd)

joinCrosswalk :: Bicrosswalk p => Align f => (a -> f b) -> Join p a -> f (Join p b)
joinCrosswalk f = fmap Join . bicrosswalk f f . runJoin

instance Listable BranchElement where
  tiers = oneof [ (\ key -> Child key `mapT` joinTheseOf (contents key)) `concatMapT` key
                , Margin `mapT` joinTheseOf (Text.singleton `mapT` padding '-') ]
    where key = Text.singleton `mapT` [['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']]
          contents key = (wrap key . Text.singleton) `mapT` padding '*'
          wrap key contents = "(" <> key <> contents <> ")" :: Text
          padding :: Char -> [Tier Char]
          padding char = frequency [ (10, [[char]])
                                   , (1, [['\n']]) ]
          joinTheseOf g = oneof [ (Join . This) `mapT` g
                                , (Join . That) `mapT` g
                                , productWith ((Join .) . These) g g ]
          frequency :: [(Int, [Tier a])] -> [Tier a]
          frequency = concatT . foldr ((\/) . pure . uncurry replicate) []
          oneof :: [[[a]]] -> [[a]]
          oneof = frequency . fmap ((,) 1)


counts :: [Join These (Int, a)] -> Both Int
counts numbered = fromMaybe 0 . getLast . mconcat . fmap Last <$> Join (unalign (runJoin . fmap Prologue.fst <$> numbered))

align :: Both Source.Source -> ConstructibleFree (Syntax Text) (Patch (Term (Syntax Text) (Record '[Range]))) (Both (Record '[Range])) -> PrettyDiff (SplitDiff [] (Record '[Range]))
align sources = PrettyDiff sources . fmap (fmap (getRange &&& identity)) . alignDiff sources . deconstruct

info :: Int -> Int -> Record '[Range]
info start end = Range start end :. Nil

prettyDiff :: Both Source.Source -> [Join These (ConstructibleFree [] (SplitPatch (Term [] (Record '[Range]))) (Record '[Range]))] -> PrettyDiff (SplitDiff [] (Record '[Range]))
prettyDiff sources = PrettyDiff sources . fmap (fmap ((getRange &&& identity) . deconstruct))

data PrettyDiff a = PrettyDiff { unPrettySources :: Both Source.Source, unPrettyLines :: [Join These (Range, a)] }
  deriving Eq

instance Show (PrettyDiff a) where
  showsPrec _ (PrettyDiff sources lines) = (prettyPrinted ++) -- . (("\n" ++ show lines) ++)
    where prettyPrinted = showLine (maximum (0 : (maximum . fmap length <$> shownLines))) <$> shownLines >>= ('\n':)
          shownLines = catMaybes $ toBoth <$> lines
          showLine n line = uncurry ((<>) . (++ " | ")) (fromThese (replicate n ' ') (replicate n ' ') (runJoin (pad n <$> line)))
          showDiff (range, _) = filter (/= '\n') . Text.unpack . Source.toText . Source.slice range
          pad n string = (<>) (take n string) (replicate (max 0 (n - length string)) ' ')
          toBoth them = showDiff <$> them `applyThese` modifyJoin (uncurry These) sources

newtype ConstructibleFree f patch annotation = ConstructibleFree { deconstruct :: Free (CofreeF f annotation) patch }


class PatchConstructible p where
  insert :: Term (Syntax Text) (Record '[Range]) -> p
  delete :: Term (Syntax Text) (Record '[Range]) -> p

instance PatchConstructible (Patch (Term (Syntax Text) (Record '[Range]))) where
  insert = Insert
  delete = Delete

instance PatchConstructible (SplitPatch (Term (Syntax Text) (Record '[Range]))) where
  insert = SplitInsert
  delete = SplitDelete

instance PatchConstructible (SplitPatch (Term [] (Record '[Range]))) where
  insert = SplitInsert . hoistCofree toList
  delete = SplitDelete . hoistCofree toList

instance (Functor f, PatchConstructible patch) => PatchConstructible (ConstructibleFree f patch annotation) where
  insert = ConstructibleFree . pure . insert
  delete = ConstructibleFree . pure . delete

class SyntaxConstructible s where
  leaf :: annotation -> Text -> s annotation
  branch :: annotation -> [s annotation] -> s annotation

instance SyntaxConstructible (ConstructibleFree (Syntax Text) patch) where
  leaf info = ConstructibleFree . free . Free . (info :<) . Leaf
  branch info = ConstructibleFree . free . Free . (info :<) . Indexed . fmap deconstruct

instance SyntaxConstructible (ConstructibleFree [] patch) where
  leaf info = ConstructibleFree . free . Free . (info :<) . const []
  branch info = ConstructibleFree . free . Free . (info :<) . fmap deconstruct

instance SyntaxConstructible (Cofree (Syntax Text)) where
  info `leaf` value = cofree $ info :< Leaf value
  info `branch` children = cofree $ info :< Indexed children

instance SyntaxConstructible (Cofree []) where
  info `leaf` _ = cofree $ info :< []
  info `branch` children = cofree $ info :< children
