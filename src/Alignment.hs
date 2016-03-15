{-# LANGUAGE RankNTypes #-}
module Alignment
( hasChanges
, linesInRangeOfSource
, numberedRows
, splitAbstractedTerm
, splitDiffByLines
, Row
) where

import Category
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Adjoined
import Data.Align
import Data.Bifunctor.Join
import Data.Bifunctor.These
import Data.Coalescent
import Data.Copointed
import Data.Foldable
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Diff
import Line hiding (maybeFirst)
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import Source hiding (fromList, uncons)
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
splitDiffByLines sources = fmap (bothOfThese (pure $ Line []) . runJoin) . toList . iter (\ (Annotated infos syntax) -> splitAbstractedTerm ((Free .) . Annotated) (joinBoth sources) (joinBoth infos) syntax) . fmap (splitPatchByLines $ joinBoth sources)
  where joinBoth = Join . runBothWith These

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Join These (Source Char) -> Patch (Term leaf Info) -> Adjoined (Join These (Line (SplitDiff leaf Info, Range)))
splitPatchByLines sources patch = wrapTermInPatch <$> splitAndFoldTerm (Join $ unPatch patch)
    where splitAndFoldTerm (Join (This deleted)) = cata (splitAbstractedTerm (:<) sources) (Join . This <$> deleted)
          splitAndFoldTerm (Join (That inserted)) = cata (splitAbstractedTerm (:<) sources) (Join . That <$> inserted)
          splitAndFoldTerm (Join (These deleted inserted)) = alignWith combine (cata (splitAbstractedTerm (:<) sources) (Join . This <$> deleted))
                                                                               (cata (splitAbstractedTerm (:<) sources) (Join . That <$> inserted))
          combine (These (Join a) (Join b)) | Just a <- maybeFirst a, Just b <- maybeSecond b = Join $ These a b
                                            | Just a <- maybeFirst a = Join $ This a
                                            | Just b <- maybeSecond b = Join $ That b
                                            | otherwise = Join $ These (Line []) (Line [])
          combine (This (Join a)) = Join a
          combine (That (Join b)) = Join b
          wrapTermInPatch = fmap (fmap (first (Pure . constructor patch)))
          constructor (Replace _ _) = SplitReplace
          constructor (Insert _) = SplitInsert
          constructor (Delete _) = SplitDelete

-- | Split a term comprised of an Info & Syntax up into one `outTerm` (abstracted by an alignment function & constructor) per line in `Source`.
splitAbstractedTerm :: (Info -> Syntax leaf outTerm -> outTerm) -> Join These (Source Char) -> Join These Info -> Syntax leaf (Adjoined (Join These (Line (outTerm, Range)))) -> Adjoined (Join These (Line (outTerm, Range)))
splitAbstractedTerm makeTerm sources infos syntax = case syntax of
  Leaf a -> sequenceL $ fmap <$> ((\ categories -> fmap (\ range -> (makeTerm (Info range categories) (Leaf a), range))) <$> (Diff.categories <$> infos)) <*> (fmap fromList $ linesInRangeOfSource <$> (characterRange <$> infos) <*> sources)
  Indexed children -> adjoinChildren sources infos (constructor (Indexed . fmap runIdentity)) (Identity <$> children)
  Fixed children -> adjoinChildren sources infos (constructor (Fixed . fmap runIdentity)) (Identity <$> children)
  Keyed children -> adjoinChildren sources infos (constructor (Keyed . Map.fromList)) (Map.toList children)
  where constructor with info = makeTerm info . with

-- | Adjoin a branch term’s lines, wrapping children & context in branch nodes using a constructor.
adjoinChildren :: (Copointed c, Functor c) => Join These (Source Char) -> Join These Info -> (Info -> [c a] -> outTerm) -> [c (Adjoined (Join These (Line (a, Range))))] -> Adjoined (Join These (Line (outTerm, Range)))
adjoinChildren sources infos constructor children = wrap <$> leadingContext <> lines
  where (lines, next) = foldr (childLines sources) (mempty, end <$> ranges) children
        ranges = characterRange <$> infos
        categories = Diff.categories <$> infos
        leadingContext = sequenceL $ fromList . fmap (fmap ((,) Nothing)) <$> (linesInRangeOfSource <$> (Range <$> (start <$> ranges) <*> next) <*> sources)
        wrap = (wrapLineContents <$> (makeBranchTerm constructor <$> categories <*> next) <*>)
        makeBranchTerm constructor categories next children = let range = unionRangesFrom (rangeAt next) $ Prelude.snd <$> children in
          (constructor (Info range categories) . catMaybes . toList $ Prelude.fst <$> children, range)

-- | Accumulate the lines of and between a branch term’s children.
childLines :: (Copointed c, Functor c) => Join These (Source Char) -> c (Adjoined (Join These (Line (a, Range)))) -> (Adjoined (Join These (Line (Maybe (c a), Range))), Join These Int) -> (Adjoined (Join These (Line (Maybe (c a), Range))), Join These Int)
-- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
childLines sources child (followingLines, next) | or $ (>) . end <$> childRanges <*> next = (followingLines, next)
                                                | otherwise =
  ((placeChildAndRangeInContainer <$> copoint child)
  <> sequenceL (fromList . pairWithNothing <$> trailingContextLines)
  <> followingLines, start <$> childRanges)
  where pairWithNothing = fmap (fmap ((,) Nothing))
        placeChildAndRangeInContainer = fmap (fmap (first (Just . (<$ child))))
        trailingContextLines = linesInRangeOfSource <$> rangeUntilNext <*> sources
        rangeUntilNext = (Range <$> (end <$> childRanges) <*> next)
        childRanges = unionLineRangesFrom <$> (rangeAt <$> next) <*> sequenceA (copoint child)

-- | Produce open/closed lines for the portion of the source spanned by a range.
linesInRangeOfSource :: Range -> Source Char -> [Line Range]
linesInRangeOfSource range source = pureBy (openRange source) <$> actualLineRanges range source

-- | Compute the union of the ranges in a list of ranged lines.
unionLineRangesFrom :: (Foldable t, Monad t) => Range -> t (Line (a, Range)) -> Range
unionLineRangesFrom start lines = unionRangesFrom start (concat (fmap Prelude.snd . unLine <$> lines))

-- | Does this Range in this Source end with a newline?
openRange :: Source Char -> Range -> Bool
openRange source range = (at source <$> maybeLastIndex range) /= Just '\n'

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Both (Line a)
