{-# LANGUAGE DataKinds #-}
module AlignmentSpec where

import Alignment
import Control.Arrow ((&&&))
import Control.Monad.Free (wrap)
import Control.Monad.State
import Data.Align hiding (align)
import Data.Bifunctor
import Data.Bifunctor.Join
import Data.Diff
import Data.Functor.Both as Both hiding (fst, snd)
import Data.Functor.Listable
import Data.List (nub, sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid hiding ((<>))
import Data.Range
import Data.Record
import Data.Semigroup ((<>))
import qualified Data.Source as Source
import Data.SplitDiff
import Data.Term
import qualified Data.Text as Text
import Data.These
import Syntax
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck
import GHC.Show (Show(..))

spec :: Spec
spec = parallel $ do
  describe "alignBranch" $ do
    it "produces symmetrical context" $
      alignBranch getRange ([] :: [Join These (SplitDiff Syntax (Record '[Range]))]) (both [Range 0 2, Range 2 4] [Range 0 2, Range 2 4]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 2, []))
        , Join (These (Range 2 4, [])
                      (Range 2 4, []))
        ]

    it "produces asymmetrical context" $
      alignBranch getRange ([] :: [Join These (SplitDiff Syntax (Record '[Range]))]) (both [Range 0 2, Range 2 4] [Range 0 1]) `shouldBe`
        [ Join (These (Range 0 2, [])
                      (Range 0 1, []))
        , Join (This  (Range 2 4, []))
        ]

    prop "covers every input line" $
      \ elements -> let (_, children, ranges) = toAlignBranchInputs elements in
        join <$> traverse (modifyJoin (fromThese [] []) . fmap (pure . fst)) (alignBranch snd children ranges) `shouldBe` ranges

    prop "covers every input child" $
      \ elements -> let (_, children, ranges) = toAlignBranchInputs elements in
        sort (nub (keysOfAlignedChildren (alignBranch snd children ranges))) `shouldBe` sort (nub (catMaybes (branchElementKey <$> elements)))

    prop "covers every line of every input child" $
      \ elements -> let (_, children, ranges) = toAlignBranchInputs elements in
        sort (keysOfAlignedChildren (alignBranch snd children ranges)) `shouldBe` sort (do
          line <- children
          these (pure . fst) (pure . fst) (\ (k1, _) (k2, _) -> [ k1, k2 ]) . runJoin $ line)

  describe "alignDiff" $ do
    it "aligns identical branches on a single line" $
      let sources = both (Source.fromText "[ foo ]") (Source.fromText "[ foo ]") in
      align sources ((info 0 7, info 0 7) `merge` Indexed [ (info 2 5, info 2 5) `merge` Leaf "foo" ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 7 `In` [ wrap $ info 2 5 `In` [] ])
                      (wrap $ info 0 7 `In` [ wrap $ info 2 5 `In` [] ])) ]

    it "aligns identical branches spanning multiple lines" $
      let sources = both (Source.fromText "[\nfoo\n]") (Source.fromText "[\nfoo\n]") in
      align sources ((info 0 7, info 0 7) `merge` Indexed [ (info 2 5, info 2 5) `merge` Leaf "foo" ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 2 `In` [])
                      (wrap $ info 0 2 `In` []))
        , Join (These (wrap $ info 2 6 `In` [ wrap $ info 2 5 `In` [] ])
                      (wrap $ info 2 6 `In` [ wrap $ info 2 5 `In` [] ]))
        , Join (These (wrap $ info 6 7 `In` [])
                      (wrap $ info 6 7 `In` []))
        ]

    it "aligns reformatted branches" $
      let sources = both (Source.fromText "[ foo ]") (Source.fromText "[\nfoo\n]") in
      align sources ((info 0 7, info 0 7) `merge` Indexed [ (info 2 5, info 2 5) `merge` Leaf "foo" ]) `shouldBe` prettyDiff sources
        [ Join (That  (wrap $ info 0 2 `In` []))
        , Join (These (wrap $ info 0 7 `In` [ wrap $ info 2 5 `In` [] ])
                      (wrap $ info 2 6 `In` [ wrap $ info 2 5 `In` [] ]))
        , Join (That  (wrap $ info 6 7 `In` []))
        ]

    it "aligns nodes following reformatted branches" $
      let sources = both (Source.fromText "[ foo ]\nbar\n") (Source.fromText "[\nfoo\n]\nbar\n") in
      align sources ((info 0 12, info 0 12) `merge` Indexed [ (info 0 7, info 0 7) `merge` Indexed [ (info 2 5, info 2 5) `merge` Leaf "foo" ], (info 8 11, info 8 11) `merge` Leaf "bar" ]) `shouldBe` prettyDiff sources
        [ Join (That  (wrap $ info 0 2   `In` [ wrap $ info 0 2  `In` [] ]))
        , Join (These (wrap $ info 0 8   `In` [ wrap $ info 0 7  `In` [ wrap $ info 2 5 `In` [] ] ])
                      (wrap $ info 2 6   `In` [ wrap $ info 2 6  `In` [ wrap $ info 2 5 `In` [] ] ]))
        , Join (That  (wrap $ info 6 8   `In` [ wrap $ info 6 7  `In` [] ]))
        , Join (These (wrap $ info 8 12  `In` [ wrap $ info 8 11 `In` [] ])
                      (wrap $ info 8 12  `In` [ wrap $ info 8 11 `In` [] ]))
        , Join (These (wrap $ info 12 12 `In` [])
                      (wrap $ info 12 12 `In` []))
        ]

    it "aligns identical branches with multiple children on the same line" $
      let sources = pure (Source.fromText "[ foo, bar ]") in
      align sources ((info 0 12, info 0 12) `merge` Indexed [ (info 2 5, info 2 5) `merge` Leaf "foo", (info 7 10, info 7 10) `merge` Leaf "bar" ]) `shouldBe` prettyDiff sources
        [ Join (runBothWith These (pure (wrap $ info 0 12 `In` [ wrap $ info 2 5 `In` [], wrap $ info 7 10 `In` [] ])) ) ]

    it "aligns insertions" $
      let sources = both (Source.fromText "a") (Source.fromText "a\nb") in
      align sources ((info 0 1, info 0 3) `merge` Indexed [ (info 0 1, info 0 1) `merge` Leaf "a", inserting (Term (info 2 3 `In` Leaf "b")) ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 1 `In` [ wrap $ info 0 1 `In` [] ])
                      (wrap $ info 0 2 `In` [ wrap $ info 0 1 `In` [] ]))
        , Join (That  (wrap $ info 2 3 `In` [ pure (SplitInsert (Term (info 2 3 `In` []))) ]))
        ]

    it "aligns total insertions" $
      let sources = both (Source.fromText "") (Source.fromText "a") in
      align sources (inserting (Term (info 0 1 `In` Leaf "a"))) `shouldBe` prettyDiff sources
        [ Join (That (pure (SplitInsert (Term (info 0 1 `In` []))))) ]

    it "aligns insertions into empty branches" $
      let sources = both (Source.fromText "[ ]") (Source.fromText "[a]") in
      align sources ((info 0 3, info 0 3) `merge` Indexed [ inserting (Term (info 1 2 `In` Leaf "a")) ]) `shouldBe` prettyDiff sources
        [ Join (That  (wrap $ info 0 3 `In` [ pure (SplitInsert (Term (info 1 2 `In` []))) ]))
        , Join (This  (wrap $ info 0 3 `In` []))
        ]

    it "aligns symmetrically following insertions" $
      let sources = both (Source.fromText "a\nc") (Source.fromText "a\nb\nc") in
      align sources ((info 0 3, info 0 5) `merge` Indexed [ (info 0 1, info 0 1) `merge` Leaf "a", inserting (Term (info 2 3 `In` Leaf "b")), (info 2 3, info 4 5) `merge` Leaf "c" ])
        `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 2 `In` [ wrap $ info 0 1 `In` [] ])
                      (wrap $ info 0 2 `In` [ wrap $ info 0 1 `In` [] ]))
        , Join (That  (wrap $ info 2 4 `In` [ pure (SplitInsert (Term (info 2 3 `In` []))) ]))
        , Join (These (wrap $ info 2 3 `In` [ wrap $ info 2 3 `In` [] ])
                      (wrap $ info 4 5 `In` [ wrap $ info 4 5 `In` [] ]))
        ]

    it "symmetrical nodes force the alignment of asymmetrical nodes on both sides" $
      let sources = both (Source.fromText "[ a, b ]") (Source.fromText "[ b, c ]") in
      align sources ((info 0 8, info 0 8) `merge` Indexed [ deleting (Term (info 2 3 `In` Leaf "a")), (info 5 6, info 2 3) `merge` Leaf "b", inserting (Term (info 5 6 `In` Leaf "c")) ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 8 `In` [ pure (SplitDelete (Term (info 2 3 `In` []))), wrap $ info 5 6 `In` [] ])
                      (wrap $ info 0 8 `In` [ wrap $ info 2 3 `In` [], pure (SplitInsert (Term (info 5 6 `In` []))) ])) ]

    it "when one of two symmetrical nodes must be split, splits the latter" $
      let sources = both (Source.fromText "[ a, b ]") (Source.fromText "[ a\n, b\n]") in
      align sources ((info 0 8, info 0 9) `merge` Indexed [ (info 2 3, info 2 3) `merge` Leaf "a", (info 5 6, info 6 7) `merge` Leaf "b" ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 8 `In` [ wrap $ info 2 3 `In` [], wrap $ info 5 6 `In` [] ])
                      (wrap $ info 0 4 `In` [ wrap $ info 2 3 `In` [] ]))
        , Join (That  (wrap $ info 4 8 `In` [ wrap $ info 6 7 `In` [] ]))
        , Join (That  (wrap $ info 8 9 `In` []))
        ]

    it "aligns deletions before insertions" $
      let sources = both (Source.fromText "[ a ]") (Source.fromText "[ b ]") in
      align sources ((info 0 5, info 0 5) `merge` Indexed [ deleting (Term (info 2 3 `In` Leaf "a")), inserting (Term (info 2 3 `In` Leaf "b")) ]) `shouldBe` prettyDiff sources
        [ Join (This  (wrap $ info 0 5 `In` [ pure (SplitDelete (Term (info 2 3 `In` []))) ]))
        , Join (That  (wrap $ info 0 5 `In` [ pure (SplitInsert (Term (info 2 3 `In` []))) ]))
        ]

    it "aligns context-only lines symmetrically" $
      let sources = both (Source.fromText "[\n  a\n,\n  b\n]") (Source.fromText "[\n  a, b\n\n\n]") in
      align sources ((info 0 13, info 0 12) `merge` Indexed [ (info 4 5, info 4 5) `merge` Leaf "a", (info 10 11, info 7 8) `merge` Leaf "b" ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 2 `In` [])
                      (wrap $ info 0 2 `In` []))
        , Join (These (wrap $ info 2 6 `In` [ wrap $ info 4 5 `In` [] ])
                      (wrap $ info 2 9 `In` [ wrap $ info 4 5 `In` [], wrap $ info 7 8 `In` [] ]))
        , Join (These (wrap $ info 6 8 `In` [])
                      (wrap $ info 9 10 `In` []))
        , Join (This  (wrap $ info 8 12 `In` [ wrap $ info 10 11 `In` [] ]))
        , Join (These (wrap $ info 12 13 `In` [])
                      (wrap $ info 10 11 `In` []))
        , Join (That  (wrap $ info 11 12 `In` []))
        ]

    it "aligns asymmetrical nodes preceding their symmetrical siblings conservatively" $
      let sources = both (Source.fromText "[ b, c ]") (Source.fromText "[ a\n, c\n]") in
      align sources ((info 0 8, info 0 9) `merge` Indexed [ inserting (Term (info 2 3 `In` Leaf "a")), deleting (Term (info 2 3 `In` Leaf "b")), (info 5 6, info 6 7) `merge` Leaf "c" ]) `shouldBe` prettyDiff sources
        [ Join (That  (wrap $ info 0 4 `In` [ pure (SplitInsert (Term (info 2 3 `In` []))) ]))
        , Join (These (wrap $ info 0 8 `In` [ pure (SplitDelete (Term (info 2 3 `In` []))), wrap $ info 5 6 `In` [] ])
                      (wrap $ info 4 8 `In` [ wrap $ info 6 7 `In` [] ]))
        , Join (That  (wrap $ info 8 9 `In` []))
        ]

    it "aligns symmetrical reformatted nodes" $
      let sources = both (Source.fromText "a [ b ]\nc") (Source.fromText "a [\nb\n]\nc") in
      align sources ((info 0 9, info 0 9) `merge` Indexed [ (info 0 1, info 0 1) `merge` Leaf "a", (info 2 7, info 2 7) `merge` Indexed [ (info 4 5, info 4 5) `merge` Leaf "b" ], (info 8 9, info 8 9) `merge` Leaf "c" ]) `shouldBe` prettyDiff sources
        [ Join (These (wrap $ info 0 8 `In` [ wrap $ info 0 1 `In` [], wrap $ info 2 7 `In` [ wrap $ info 4 5 `In` [] ] ])
                      (wrap $ info 0 4 `In` [ wrap $ info 0 1 `In` [], wrap $ info 2 4 `In` [] ]))
        , Join (That  (wrap $ info 4 6 `In` [ wrap $ info 4 6 `In` [ wrap $ info 4 5 `In` [] ] ]))
        , Join (That  (wrap $ info 6 8 `In` [ wrap $ info 6 7 `In` [] ]))
        , Join (These (wrap $ info 8 9 `In` [ wrap $ info 8 9 `In` [] ])
                      (wrap $ info 8 9 `In` [ wrap $ info 8 9 `In` [] ]))
        ]

  describe "numberedRows" $ do
    prop "counts only non-empty values" $
      \ xs -> counts (numberedRows (unListableF <$> xs :: [Join These Char])) `shouldBe` length . catMaybes <$> Join (unalign (runJoin . unListableF <$> xs))

data BranchElement
  = Child Text.Text (Join These Text.Text)
  | Margin (Join These Text.Text)
  deriving Show

branchElementKey :: BranchElement -> Maybe Text.Text
branchElementKey (Child key _) = Just key
branchElementKey _ = Nothing

toAlignBranchInputs :: [BranchElement] -> (Both Source.Source, [Join These (Text.Text, Range)], Both [Range])
toAlignBranchInputs elements = (sources, join . (`evalState` both 0 0) . traverse go $ elements, ranges)
  where go :: BranchElement -> State (Both Int) [Join These (Text.Text, Range)]
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

keysOfAlignedChildren :: [Join These (Range, [(Text.Text, Range)])] -> [Text.Text]
keysOfAlignedChildren lines = lines >>= these id id (<>) . runJoin . fmap (fmap fst . snd)

joinCrosswalk :: Bicrosswalk p => Align f => (a -> f b) -> Join p a -> f (Join p b)
joinCrosswalk f = fmap Join . bicrosswalk f f . runJoin

instance Listable BranchElement where
  tiers = oneof [ (\ key -> Child key `mapT` joinTheseOf (contents key)) `concatMapT` key
                , Margin `mapT` joinTheseOf (Text.singleton `mapT` padding '-') ]
    where key = Text.singleton `mapT` [['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']]
          contents key = (wrap key . Text.singleton) `mapT` padding '*'
          wrap key contents = "(" <> key <> contents <> ")" :: Text.Text
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
counts numbered = fromMaybe 0 . getLast . mconcat . fmap Last <$> Join (unalign (runJoin . fmap fst <$> numbered))

align :: Both Source.Source -> Diff Syntax (Record '[Range]) (Record '[Range]) -> PrettyDiff (SplitDiff [] (Record '[Range]))
align sources = PrettyDiff sources . fmap (fmap (getRange &&& id)) . alignDiff sources

info :: Int -> Int -> Record '[Range]
info start end = Range start end :. Nil

prettyDiff :: Both Source.Source -> [Join These (SplitDiff [] (Record '[Range]))] -> PrettyDiff (SplitDiff [] (Record '[Range]))
prettyDiff sources = PrettyDiff sources . fmap (fmap ((getRange &&& id)))

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
