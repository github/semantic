module Alignment where

import Category
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Copointed
import Data.Foldable (foldl')
import Data.Functor.Both
import Data.Functor.Identity
import qualified Data.List as List
import Data.Maybe
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import Diff
import Line
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import Row
import Source hiding ((++))
import SplitDiff
import Syntax
import Term

-- | Assign line numbers to the lines on each side of a list of rows.
numberedRows :: [Row a] -> [Both (Int, Line a)]
numberedRows = foldl' numberRows []
  where numberRows rows row = ((,) <$> ((+) <$> count rows <*> (valueOf <$> unRow row)) <*> unRow row) : rows
        count = maybe (pure 0) (fmap Prelude.fst) . maybeFirst
        valueOf EmptyLine = 0
        valueOf _ = 1

-- | Determine whether a line contains any patches.
hasChanges :: Line (SplitDiff leaf Info) -> Bool
hasChanges = or . fmap (or . (True <$))

-- | Split a diff, which may span multiple lines, into rows of split diffs.
splitDiffByLines :: Diff leaf Info -> Both (Source Char) -> [Row (SplitDiff leaf Info, Range)]
splitDiffByLines diff sources = case diff of
  Free (Annotated annotation syntax) -> splitAnnotatedByLines sources (ranges annotation) (Diff.categories <$> annotation) syntax
  Pure patch -> splitPatchByLines patch sources
  where ranges annotations = characterRange <$> annotations

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Patch (Term leaf Info) -> Both (Source Char) -> [Row (SplitDiff leaf Info, Range)]
splitPatchByLines patch sources = zipWithDefaults makeRow (pure mempty) $ fmap (fmap (first (Pure . constructor patch))) <$> lines
    where lines = (maybe [] . splitAbstractedTerm copoint unwrap (:<) <$> sources) <*> unPatch patch
          constructor (Replace _ _) = SplitReplace
          constructor (Insert _) = SplitInsert
          constructor (Delete _) = SplitDelete

-- | Split an `inTerm` (abstracted by two destructors) up into one `outTerm` (abstracted by a constructor) per line in `Source`.
splitAbstractedTerm :: (inTerm -> Info) -> (inTerm -> Syntax leaf inTerm) -> (Info -> Syntax leaf outTerm -> outTerm) -> Source Char -> inTerm -> [Line (outTerm, Range)]
splitAbstractedTerm getInfo getSyntax makeTerm source term = case getSyntax term of
  Leaf a -> pure . ((`makeTerm` Leaf a) . (`Info` (Diff.categories (getInfo term))) &&& id) <$> actualLineRanges (characterRange (getInfo term)) source
  Indexed children -> adjoinChildLines (Indexed . fmap (Prelude.fst . copoint)) (Identity <$> children)
  Fixed children -> adjoinChildLines (Fixed . fmap (Prelude.fst . copoint)) (Identity <$> children)
  Keyed children -> adjoinChildLines (Keyed . fmap Prelude.fst . Map.fromList) (Map.toList children)
  where adjoin = reverse . foldl (adjoinLinesBy (openRangePair source)) []

        adjoinChildLines constructor children = let (lines, previous) = foldl childLines ([], start (characterRange (getInfo term))) children in
          fmap (wrapLineContents (makeBranchTerm (\ info -> makeTerm info . constructor) (Diff.categories (getInfo term)) previous)) . adjoin $ lines ++ (pure . (,) Nothing <$> actualLineRanges (Range previous $ end (characterRange (getInfo term))) source)

        childLines (lines, previous) child = let childLines = splitAbstractedTerm getInfo getSyntax makeTerm source (copoint child) in
          (adjoin $ lines ++ (pure . (,) Nothing <$> actualLineRanges (Range previous $ start (unionLineRangesFrom (rangeAt previous) childLines)) source) ++ (fmap (flip (,) (unionLineRangesFrom (rangeAt previous) childLines) . Just . (<$ child)) <$> childLines), end (unionLineRangesFrom (rangeAt previous) childLines))

-- | Split a annotated diff into rows of split diffs.
splitAnnotatedByLines :: Both (Source Char) -> Both Range -> Both (Set.Set Category) -> Syntax leaf (Diff leaf Info) -> [Row (SplitDiff leaf Info, Range)]
splitAnnotatedByLines sources ranges categories syntax = case syntax of
  Leaf a -> zipWithDefaults makeRow (pure mempty) $ fmap <$> ((\ categories range -> pure (Free (Annotated (Info range categories) (Leaf a)), range)) <$> categories) <*> (actualLineRanges <$> ranges <*> sources)
  Indexed children -> adjoinChildRows (Indexed . fmap copoint) (Identity <$> children)
  Fixed children -> adjoinChildRows (Fixed . fmap copoint) (Identity <$> children)
  Keyed children -> adjoinChildRows (Keyed . Map.fromList) (List.sortOn (diffRanges . Prelude.snd) $ Map.toList children)
  where adjoin :: [Row (Maybe (f (SplitDiff leaf Info)), Range)] -> [Row (Maybe (f (SplitDiff leaf Info)), Range)]
        adjoin = reverse . foldl (adjoinRowsBy (openRangePair <$> sources)) []

        adjoinChildRows :: (Copointed f, Functor f) => ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> [f (Diff leaf Info)] -> [Row (SplitDiff leaf Info, Range)]
        adjoinChildRows constructor children = let (rows, previous) = foldl childRows ([], start <$> ranges) children in
          fmap (wrapRowContents (makeBranchTerm (\ info -> Free . Annotated info . constructor) <$> categories <*> previous)) . adjoin $ rows ++ (zipWithDefaults makeRow (pure mempty) (fmap (pure . (,) Nothing) <$> (actualLineRanges <$> (makeRanges previous (end <$> ranges)) <*> sources)))

        childRows :: (Copointed f, Functor f) => ([Row (Maybe (f (SplitDiff leaf Info)), Range)], Both Int) -> f (Diff leaf Info) -> ([Row (Maybe (f (SplitDiff leaf Info)), Range)], Both Int)
        childRows (rows, previous) child = let childRows = splitDiffByLines (copoint child) sources in
          -- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
          if or $ (<) . start <$> (unionLineRangesFrom <$> (rangeAt <$> previous) <*> sequenceA (unRow <$> childRows)) <*> previous
            then (rows, previous)
            else (adjoin $ rows ++ zipWithDefaults makeRow (pure mempty) (fmap (pure . (,) Nothing) <$> (actualLineRanges <$> (makeRanges previous (start <$> (unionLineRangesFrom <$> (rangeAt <$> previous) <*> sequenceA (unRow <$> childRows)))) <*> sources)) ++ (fmap (first (Just . (<$ child))) <$> childRows), end <$> (unionLineRangesFrom <$> (rangeAt <$> previous) <*> sequenceA (unRow <$> childRows)))

-- | Wrap a list of child terms in a branch.
makeBranchTerm :: (Info -> [inTerm] -> outTerm) -> Set.Set Category -> Int -> [(Maybe inTerm, Range)] -> (outTerm, Range)
makeBranchTerm constructor categories previous children = (constructor (Info range categories) . catMaybes $ Prelude.fst <$> children, range)
  where range = unionRangesFrom (rangeAt previous) $ Prelude.snd <$> children

-- | Compute the union of the ranges in a list of ranged lines.
unionLineRangesFrom :: Range -> [Line (a, Range)] -> Range
unionLineRangesFrom start lines = unionRangesFrom start (lines >>= (fmap Prelude.snd . unLine))

-- | Construct Both Ranges from Both starting and ending indices.
makeRanges :: Both Int -> Both Int -> Both Range
makeRanges a b = Range <$> a <*> b

-- | Produces the starting indices of a diff.
diffRanges :: Diff leaf Info -> Both (Maybe Range)
diffRanges (Free (Annotated infos _)) = Just . characterRange <$> infos
diffRanges (Pure patch) = fmap (characterRange . copoint) <$> unPatch patch

-- | MaybeOpen test for (Range, a) pairs.
openRangePair :: Source Char -> MaybeOpen (a, Range)
openRangePair source pair = pair <$ openRange source (Prelude.snd pair)

-- | Given a source and a range, returns nothing if it ends with a `\n`;
-- | otherwise returns the range.
openRange :: Source Char -> MaybeOpen Range
openRange source range = case (source `at`) <$> maybeLastIndex range of
  Just '\n' -> Nothing
  _ -> Just range
