{-# LANGUAGE RankNTypes #-}
module Alignment where

import Category
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Copointed
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.Maybe
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
  where countUp from (row : rows) = ((,) <$> from <*> row) : countUp ((+) <$> from <*> (lineIncrement <$> row)) rows
        countUp _ [] = []

-- | Determine whether a line contains any patches.
hasChanges :: Line (SplitDiff leaf Info) -> Bool
hasChanges = or . fmap (or . (True <$))

-- | Split a diff, which may span multiple lines, into rows of split diffs paired with the Range of characters spanned by that Row on each side of the diff.
splitDiffByLines :: Both (Source Char) -> Diff leaf Info -> [Row (SplitDiff leaf Info, Range)]
splitDiffByLines sources = iter (\ (Annotated infos syntax) -> splitAbstractedTerm (zipDefaults mempty) ((Free .) . Annotated) sources infos syntax) . fmap (splitPatchByLines sources)

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Both (Source Char) -> Patch (Term leaf Info) -> [Row (SplitDiff leaf Info, Range)]
splitPatchByLines sources patch = zipDefaults mempty $ fmap (fmap (first (Pure . constructor patch)) . runIdentity) <$> lines
    where lines = maybe [] . cata . splitAbstractedTerm sequenceA (:<) <$> (Identity <$> sources) <*> (fmap (fmap Identity) <$> unPatch patch)
          constructor (Replace _ _) = SplitReplace
          constructor (Insert _) = SplitInsert
          constructor (Delete _) = SplitDelete

-- | Split a term comprised of an Info & Syntax up into one `outTerm` (abstracted by an alignment function & constructor) per line in `Source`.
splitAbstractedTerm :: (Applicative f, Foldable f) => (forall b. Monoid b => f [b] -> [f b]) -> (Info -> Syntax leaf outTerm -> outTerm) -> f (Source Char) -> f Info -> Syntax leaf [f (Line (outTerm, Range))] -> [f (Line (outTerm, Range))]
splitAbstractedTerm align makeTerm sources infos syntax = case syntax of
  Leaf a -> align $ fmap <$> ((\ categories -> fmap (\ range -> (makeTerm (Info range categories) (Leaf a), range))) <$> (Diff.categories <$> infos)) <*> (linesInRangeOfSource <$> (characterRange <$> infos) <*> sources)
  Indexed children -> adjoinChildren sources infos align (constructor (Indexed . fmap runIdentity)) (Identity <$> children)
  Fixed children -> adjoinChildren sources infos align (constructor (Fixed . fmap runIdentity)) (Identity <$> children)
  Keyed children -> adjoinChildren sources infos align (constructor (Keyed . Map.fromList)) (Map.toList children)
  where constructor with info = makeTerm info . with

adjoinChildren :: (Copointed c, Functor c, Applicative f, Foldable f) => f (Source Char) -> f Info -> (f [Line (Maybe (c a), Range)] -> [f (Line (Maybe (c a), Range))]) -> (Info -> [c a] -> outTerm) -> [c [f (Line (a, Range))]] -> [f (Line (outTerm, Range))]
adjoinChildren sources infos align constructor children =
  fmap wrap . foldr (adjoinRowsBy (openRangePair <$> sources) align) [] $
    align leadingContext ++ lines
  where (lines, next) = foldr (childLines sources align) ([], end <$> ranges) children
        ranges = characterRange <$> infos
        categories = Diff.categories <$> infos
        leadingContext = fmap (fmap ((,) Nothing)) <$> (linesInRangeOfSource <$> (Range <$> (start <$> ranges) <*> next) <*> sources)
        wrap = (wrapLineContents <$> (makeBranchTerm constructor <$> categories <*> next) <*>)

childLines :: (Copointed c, Functor c, Applicative f, Foldable f) => f (Source Char) -> (f [Line (Maybe (c a), Range)] -> [f (Line (Maybe (c a), Range))]) -> c [f (Line (a, Range))] -> ([f (Line (Maybe (c a), Range))], f Int) -> ([f (Line (Maybe (c a), Range))], f Int)
-- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
childLines _ _ child (lines, next) | or $ (>) . end <$> childRanges next child <*> next = (lines, next)
childLines sources align child (lines, next) =
  ((fmap (fmap (first (Just . (<$ child)))) <$> copoint child)
  ++ align (fmap (fmap ((,) Nothing)) <$> (linesInRangeOfSource <$> (Range <$> (end <$> childRanges next child) <*> next) <*> sources))
  ++ lines, start <$> childRanges next child)

childRanges :: (Copointed c, Applicative f) => f Int -> c [f (Line (a, Range))] -> f Range
childRanges next child = unionLineRangesFrom <$> (rangeAt <$> next) <*> sequenceA (copoint child)

linesInRangeOfSource :: Range -> Source Char -> [Line Range]
linesInRangeOfSource range source = (if openRange source range then Line else Closed) . pure <$> actualLineRanges range source

-- | Wrap a list of child terms in a branch.
makeBranchTerm :: (Info -> [inTerm] -> outTerm) -> Set.Set Category -> Int -> [(Maybe inTerm, Range)] -> (outTerm, Range)
makeBranchTerm constructor categories next children = (constructor (Info range categories) . catMaybes $ Prelude.fst <$> children, range)
  where range = unionRangesFrom (rangeAt next) $ Prelude.snd <$> children

-- | Compute the union of the ranges in a list of ranged lines.
unionLineRangesFrom :: Range -> [Line (a, Range)] -> Range
unionLineRangesFrom start lines = unionRangesFrom start (lines >>= (fmap Prelude.snd . unLine))

-- | Openness predicate for (Range, a) pairs.
openRangePair :: Source Char -> (a, Range) -> Bool
openRangePair source pair = openRange source (Prelude.snd pair)

-- | Does this Range in this Source end with a newline?
openRange :: Source Char -> Range -> Bool
openRange source range = (at source <$> maybeLastIndex range) /= Just '\n'

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Both (Line a)

-- | Merge open lines and prepend closed lines (as determined by a pair of functions) onto a list of rows.
adjoinRowsBy :: Applicative f => f (a -> Bool) -> (f [Line a] -> [f (Line a)]) -> f (Line a) -> [f (Line a)] -> [f (Line a)]
adjoinRowsBy _ _ row [] = [ row ]
adjoinRowsBy f align row (nextRow : rows) = align (coalesceLinesBy <$> f <*> row <*> nextRow) ++ rows
