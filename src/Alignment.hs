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
splitDiffByLines :: Diff leaf Info -> Both Int -> Both (Source Char) -> ([Row (SplitDiff leaf Info)], Both Range)
splitDiffByLines diff previous sources = case diff of
  Free (Annotated annotation syntax) -> (splitAnnotatedByLines sources (ranges annotation) (Diff.categories <$> annotation) syntax, ranges annotation)
  Pure patch -> splitPatchByLines patch previous sources
  where ranges annotations = characterRange <$> annotations

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Patch (Term leaf Info) -> Both Int -> Both (Source Char) -> ([Row (SplitDiff leaf Info)], Both Range)
splitPatchByLines patch previous sources = case patch of
  Insert term -> let (lines, range) = splitAbstractedTerm copoint unwrap (:<) (snd sources) term in
    (makeRow EmptyLine . fmap (Pure . SplitInsert) <$> lines, both (rangeAt $ fst previous) range)
  Delete term -> let (lines, range) = splitAbstractedTerm copoint unwrap (:<) (fst sources) term in
    (flip makeRow EmptyLine . fmap (Pure . SplitDelete) <$> lines, both range (rangeAt $ snd previous))
  Replace _ _ -> (zipWithDefaults makeRow (pure mempty) $ fmap (fmap (Pure . SplitReplace)) <$> lines, ranges)
    where (lines, ranges) = transpose $ maybe . (,) [] . rangeAt <$> previous <*> (splitAbstractedTerm copoint unwrap (:<) <$> sources) <*> unPatch patch

-- | Split an `inTerm` (abstracted by two destructors) up into one `outTerm` (abstracted by a constructor) per line in `Source`.
splitAbstractedTerm :: (inTerm -> Info) -> (inTerm -> Syntax leaf inTerm) -> (Info -> Syntax leaf outTerm -> outTerm) -> Source Char -> inTerm -> ([Line outTerm], Range)
splitAbstractedTerm getInfo getSyntax makeTerm source term = flip (,) (characterRange (getInfo term)) $ case getSyntax term of
  Leaf a -> pure . (`makeTerm` Leaf a) . (`Info` (Diff.categories (getInfo term))) <$> actualLineRanges (characterRange (getInfo term)) source
  Indexed children -> adjoinChildLines (Indexed . fmap copoint) (Identity <$> children)
  Fixed children -> adjoinChildLines (Fixed . fmap copoint) (Identity <$> children)
  Keyed children -> adjoinChildLines (Keyed . Map.fromList) (Map.toList children)
  where adjoin = reverse . foldl (adjoinLinesBy (openRangePair source)) []

        adjoinChildLines constructor children = let (lines, previous) = foldl childLines ([], start (characterRange (getInfo term))) children in
          fmap (wrapLineContents $ wrap constructor) . adjoin $ lines ++ (pure . flip (,) Nothing <$> actualLineRanges (Range previous $ end (characterRange (getInfo term))) source)

        wrap constructor children = (makeTerm $ Info (unionRanges $ Prelude.fst <$> children) (Diff.categories (getInfo term))) . constructor . catMaybes $ Prelude.snd <$> children

        childLines (lines, previous) child = let (childLines, childRange) = splitAbstractedTerm getInfo getSyntax makeTerm source (copoint child) in
          (adjoin $ lines ++ (pure . flip (,) Nothing <$> actualLineRanges (Range previous $ start childRange) source) ++ (fmap ((,) childRange . Just . (<$ child)) <$> childLines), end childRange)

-- | Split a annotated diff into rows of split diffs.
splitAnnotatedByLines :: Both (Source Char) -> Both Range -> Both (Set.Set Category) -> Syntax leaf (Diff leaf Info) -> [Row (SplitDiff leaf Info)]
splitAnnotatedByLines sources ranges categories syntax = case syntax of
  Leaf a -> zipWithDefaults makeRow (pure mempty) $ fmap <$> (((pure . Free . (`Annotated` Leaf a)) .) <$> flip Info <$> categories) <*> (actualLineRanges <$> ranges <*> sources)
  Indexed children -> adjoinChildRows (Indexed . fmap copoint) (Identity <$> children)
  Fixed children -> adjoinChildRows (Fixed . fmap copoint) (Identity <$> children)
  Keyed children -> adjoinChildRows (Keyed . Map.fromList) (List.sortOn (diffRanges . Prelude.snd) $ Map.toList children)
  where adjoin :: [Row (Range, Maybe (f (SplitDiff leaf Info)))] -> [Row (Range, Maybe (f (SplitDiff leaf Info)))]
        adjoin = reverse . foldl (adjoinRowsBy (openRangePair <$> sources)) []

        adjoinChildRows :: (Copointed f, Functor f) => ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> [f (Diff leaf Info)] -> [Row (SplitDiff leaf Info)]
        adjoinChildRows constructor children = let (rows, previous) = foldl childRows ([], start <$> ranges) children in
          fmap (wrapRowContents (wrap constructor <$> categories)) . adjoin $ rows ++ zipWithDefaults makeRow (pure mempty) (fmap (pure . flip (,) Nothing) <$> (actualLineRanges <$> (makeRanges previous (end <$> ranges)) <*> sources))

        wrap :: ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> Set.Set Category -> [(Range, Maybe (f (SplitDiff leaf Info)))] -> SplitDiff leaf Info
        wrap constructor categories children = Free . Annotated (Info (unionRanges $ Prelude.fst <$> children) categories) . constructor . catMaybes $ Prelude.snd <$> children

        getRange :: SplitDiff leaf Info -> Range
        getRange (Pure patch) = characterRange (copoint (getSplitTerm patch))
        getRange (Free (Annotated info _)) = characterRange info

        childRows :: (Copointed f, Functor f) => ([Row (Range, Maybe (f (SplitDiff leaf Info)))], Both Int) -> f (Diff leaf Info) -> ([Row (Range, Maybe (f (SplitDiff leaf Info)))], Both Int)
        childRows (rows, previous) child = let (childRows, childRanges) = splitDiffByLines (copoint child) previous sources in
          -- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
          if or $ (<) . start <$> childRanges <*> previous
            then (rows, previous)
            else (adjoin $ rows ++ zipWithDefaults makeRow (pure mempty) (fmap (pure . flip (,) Nothing) <$> (actualLineRanges <$> (makeRanges previous (start <$> childRanges)) <*> sources)) ++ (fmap (getRange &&& Just . (<$ child)) <$> childRows), end <$> childRanges)

        makeRanges :: Both Int -> Both Int -> Both Range
        makeRanges a b = runBothWith Range <$> sequenceA (both a b)

-- | Produces the starting indices of a diff.
diffRanges :: Diff leaf Info -> Both (Maybe Range)
diffRanges (Free (Annotated infos _)) = Just . characterRange <$> infos
diffRanges (Pure patch) = fmap (characterRange . copoint) <$> unPatch patch

-- | MaybeOpen test for (Range, a) pairs.
openRangePair :: Source Char -> MaybeOpen (Range, a)
openRangePair source pair = pair <$ openRange source (Prelude.fst pair)

-- | Given a source and a range, returns nothing if it ends with a `\n`;
-- | otherwise returns the range.
openRange :: Source Char -> MaybeOpen Range
openRange source range = case (source `at`) <$> maybeLastIndex range of
  Just '\n' -> Nothing
  _ -> Just range
