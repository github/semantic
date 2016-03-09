module Alignment where

import Category
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Copointed
import Data.Functor.Both as Both
import Data.Functor.Identity
import qualified Data.List as List
import Data.Maybe
import Data.Option
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import Diff
import Line
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import Source hiding ((++))
import SplitDiff
import Syntax
import Term

-- | Assign line numbers to the lines on each side of a list of rows.
numberedRows :: [Row a] -> [Both (Int, Line a)]
numberedRows = countUp (pure 1)
  where countUp from (row : rows) = ((,) <$> from <*> row) : countUp ((+) <$> from <*> (valueOf <$> row)) rows
        countUp _ [] = []
        valueOf (Line []) = 0
        valueOf _ = 1

-- | Determine whether a line contains any patches.
hasChanges :: Line (SplitDiff leaf Info) -> Bool
hasChanges = or . fmap (or . (True <$))

-- | Split a diff, which may span multiple lines, into rows of split diffs paired with the Range of characters spanned by that Row on each side of the diff.
splitDiffByLines :: Both (Source Char) -> Diff leaf Info -> [Row (SplitDiff leaf Info, Range)]
splitDiffByLines sources = iter (\ (Annotated info syntax) -> splitAnnotatedByLines ((Free .) . Annotated) sources info syntax) . fmap (splitPatchByLines sources)

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Both (Source Char) -> Patch (Term leaf Info) -> [Row (SplitDiff leaf Info, Range)]
splitPatchByLines sources patch = zipDefaults (pure mempty) $ fmap (fmap (first (Pure . constructor patch))) <$> lines
    where lines = maybe [] . cata . splitAbstractedTerm (:<) <$> sources <*> unPatch patch
          constructor (Replace _ _) = SplitReplace
          constructor (Insert _) = SplitInsert
          constructor (Delete _) = SplitDelete

-- | Split a term comprised of an Info & Syntax up into one `outTerm` (abstracted by a constructor) per line in `Source`.
splitAbstractedTerm :: (Info -> Syntax leaf outTerm -> outTerm) -> Source Char -> Info -> Syntax leaf [Line (outTerm, Range)] -> [Line (outTerm, Range)]
splitAbstractedTerm makeTerm source (Info range categories) syntax = case syntax of
  Leaf a -> pure . ((`makeTerm` Leaf a) . (`Info` categories) &&& id) <$> actualLineRanges range source
  Indexed children -> adjoinChildLines (Indexed . fmap copoint) (Identity . fmap Identity <$> children)
  Fixed children -> adjoinChildLines (Fixed . fmap copoint) (Identity . fmap Identity <$> children)
  Keyed children -> adjoinChildLines (Keyed . Map.fromList) (Map.toList (fmap Identity <$> children))
  where adjoinChildLines constructor children = let (lines, next) = foldr (childLines (Identity source) sequenceA) ([], Identity (end range)) children in
          fmap (wrapLineContents (makeBranchTerm (\ info -> makeTerm info . constructor) categories (runIdentity next))) . foldr (adjoinLinesBy (openRangePair source)) [] $
            (pure . (,) Nothing <$> actualLineRanges (Range (start range) (runIdentity next)) source) ++ (runIdentity <$> lines)

-- | Split an annotated diff into rows of split diffs.
splitAnnotatedByLines :: (Info -> Syntax leaf outTerm -> outTerm) -> Both (Source Char) -> Both Info -> Syntax leaf [Row (outTerm, Range)] -> [Row (outTerm, Range)]
splitAnnotatedByLines makeTerm sources infos syntax = case syntax of
  Leaf a -> zipDefaults (pure mempty) $ fmap <$> ((\ categories range -> pure (makeTerm (Info range categories) (Leaf a), range)) <$> categories) <*> (actualLineRanges <$> ranges <*> sources)
  Indexed children -> adjoinChildRows (Indexed . fmap copoint) (Identity <$> children)
  Fixed children -> adjoinChildRows (Fixed . fmap copoint) (Identity <$> children)
  Keyed children -> adjoinChildRows (Keyed . Map.fromList) (List.sortOn (rowRanges . Prelude.snd) $ Map.toList children)
  where ranges = characterRange <$> infos
        categories = Diff.categories <$> infos

        adjoinChildRows constructor children = let (rows, next) = foldr (childLines sources (zipDefaults (pure mempty))) ([], end <$> ranges) children in
          fmap (wrapLineContents <$> (makeBranchTerm (\ info -> makeTerm info . constructor) <$> categories <*> next) <*>) . foldr (adjoinRowsBy (openRangePair <$> sources)) [] $
            zipDefaults (pure mempty) (fmap (pure . (,) Nothing) <$> (actualLineRanges <$> (Range <$> (start <$> ranges) <*> next) <*> sources)) ++ rows

adjoinChildren :: (Copointed c, Functor c, Applicative f, Foldable f) => f (Source Char) -> f Info -> (f [Line (Maybe (c a), Range)] -> [f (Line (Maybe (c a), Range))]) -> (f (Line (Maybe (c a), Range)) -> [f (Line (Maybe (c a), Range))] -> [f (Line (Maybe (c a), Range))]) -> (Info -> Syntax leaf outTerm -> outTerm) -> ([c a] -> Syntax leaf outTerm) -> [c [f (Line (a, Range))]] -> [f (Line (outTerm, Range))]
adjoinChildren sources infos align adjoin makeTerm constructor children =
  fmap wrap . foldr adjoin [] $
    align leadingContext ++ lines
  where (lines, next) = foldr (childLines sources align) ([], end <$> ranges) children
        ranges = characterRange <$> infos
        categories = Diff.categories <$> infos
        leadingContext = fmap (pure . (,) Nothing) <$> (actualLineRanges <$> (Range <$> (start <$> ranges) <*> next) <*> sources)
        wrap = (wrapLineContents <$> (makeBranchTerm (\ info -> makeTerm info . constructor) <$> categories <*> next) <*>)

childLines :: (Copointed c, Functor c, Applicative f, Foldable f) => f (Source Char) -> (f [Line (Maybe (c a), Range)] -> [f (Line (Maybe (c a), Range))]) -> c [f (Line (a, Range))] -> ([f (Line (Maybe (c a), Range))], f Int) -> ([f (Line (Maybe (c a), Range))], f Int)
-- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
childLines _ _ child (lines, next) | or $ (>) . end <$> childRanges next child <*> next = (lines, next)
childLines sources align child (lines, next) =
  ((fmap (fmap (first (Just . (<$ child)))) <$> copoint child)
  ++ align (fmap (pure . (,) Nothing) <$> (actualLineRanges <$> (Range <$> (end <$> childRanges next child) <*> next) <*> sources))
  ++ lines, start <$> childRanges next child)

childRanges :: (Copointed c, Applicative f) => f Int -> c [f (Line (a, Range))] -> f Range
childRanges next child = unionLineRangesFrom <$> (rangeAt <$> next) <*> sequenceA (copoint child)

-- | Wrap a list of child terms in a branch.
makeBranchTerm :: (Info -> [inTerm] -> outTerm) -> Set.Set Category -> Int -> [(Maybe inTerm, Range)] -> (outTerm, Range)
makeBranchTerm constructor categories next children = (constructor (Info range categories) . catMaybes $ Prelude.fst <$> children, range)
  where range = unionRangesFrom (rangeAt next) $ Prelude.snd <$> children

-- | Compute the union of the ranges in a list of ranged lines.
unionLineRangesFrom :: Range -> [Line (a, Range)] -> Range
unionLineRangesFrom start lines = unionRangesFrom start (lines >>= (fmap Prelude.snd . unLine))

-- | Returns the ranges of a list of Rows.
rowRanges :: [Row (a, Range)] -> Both (Maybe Range)
rowRanges rows = maybeConcat . join <$> Both.unzip (fmap (fmap Prelude.snd . unLine) <$> rows)

-- | Openness predicate for (Range, a) pairs.
openRangePair :: Source Char -> (a, Range) -> Bool
openRangePair source pair = openRange source (Prelude.snd pair)

-- | Does this Range in this Source end with a newline?
openRange :: Source Char -> Range -> Bool
openRange source range = (at source <$> maybeLastIndex range) /= Just '\n'

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Both (Line a)

-- | Merge open lines and prepend closed lines (as determined by a pair of functions) onto a list of rows.
adjoinRowsBy :: Both (a -> Bool) -> Row a -> [Row a] -> [Row a]
adjoinRowsBy _ row [] = [ row ]
adjoinRowsBy f row (nextRow : rows) = zipDefaults mempty (coalesceLinesBy <$> f <*> row <*> nextRow) ++ rows
