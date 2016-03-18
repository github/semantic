{-# LANGUAGE RankNTypes #-}
module Alignment
( hasChanges
, linesInRangeOfSource
, numberedRows
, splitAbstractedTerm
, splitDiffByLines
, Row
) where

import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Adjoined
import Data.Align
import Data.Bifunctor.These
import Data.Coalescent
import Data.Copointed
import Data.Foldable
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import qualified Data.OrderedMap as Map
import qualified Data.Text as T
import Diff
import Line
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
splitDiffByLines sources = toList . iter (\ (Annotated infos syntax) -> splitAbstractedTerm ((Free .) . Annotated) sources infos syntax) . fmap (splitPatchByLines sources)

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Both (Source Char) -> Patch (Term leaf Info) -> Adjoined (Both (Line (SplitDiff leaf Info, Range)))
splitPatchByLines sources patch = wrapTermInPatch <$> splitAndFoldTerm (unPatch patch)
    where splitAndFoldTerm (This deleted) = tsequenceL mempty $ both (runIdentity <$> Term.cata (splitAbstractedTerm (:<) (Identity $ fst sources)) (Identity <$> deleted)) nil
          splitAndFoldTerm (That inserted) = tsequenceL mempty $ both nil (runIdentity <$> Term.cata (splitAbstractedTerm (:<) (Identity $ snd sources)) (Identity <$> inserted))
          splitAndFoldTerm (These deleted inserted) = tsequenceL mempty $ both (runIdentity <$> Term.cata (splitAbstractedTerm (:<) (Identity $ fst sources)) (Identity <$> deleted)) (runIdentity <$> Term.cata (splitAbstractedTerm (:<) (Identity $ snd sources)) (Identity <$> inserted))
          wrapTermInPatch = fmap (fmap (first (Pure . constructor patch)))
          constructor (Replace _ _) = SplitReplace
          constructor (Insert _) = SplitInsert
          constructor (Delete _) = SplitDelete

-- | Split a term comprised of an Info & Syntax up into one `outTerm` (abstracted by an alignment function & constructor) per line in `Source`.
splitAbstractedTerm :: (Applicative f, Coalescent (f (Line (Maybe (Identity outTerm), Range))), Coalescent (f (Line (Maybe (T.Text, outTerm), Range))), Foldable f, TotalCrosswalk f) => (Info -> Syntax leaf outTerm -> outTerm) -> f (Source Char) -> f Info -> Syntax leaf (Adjoined (f (Line (outTerm, Range)))) -> Adjoined (f (Line (outTerm, Range)))
splitAbstractedTerm makeTerm sources infos syntax = case syntax of
  Leaf a -> tsequenceL (pure mempty) $ fmap <$> ((\ categories -> fmap (\ range -> (makeTerm (Info range categories) (Leaf a), range))) <$> (Diff.categories <$> infos)) <*> (linesInRangeOfSource <$> (characterRange <$> infos) <*> sources)
  Indexed children -> adjoinChildren sources infos (constructor (Indexed . fmap runIdentity)) (Identity <$> children)
  Fixed children -> adjoinChildren sources infos (constructor (Fixed . fmap runIdentity)) (Identity <$> children)
  Keyed children -> adjoinChildren sources infos (constructor (Keyed . Map.fromList)) (Map.toList children)
  where constructor with info = makeTerm info . with

-- | Adjoin a branch term’s lines, wrapping children & context in branch nodes using a constructor.
adjoinChildren :: (Copointed c, Functor c, Applicative f, Coalescent (f (Line (Maybe (c a), Range))), Foldable f, TotalCrosswalk f) => f (Source Char) -> f Info -> (Info -> [c a] -> outTerm) -> [c (Adjoined (f (Line (a, Range))))] -> Adjoined (f (Line (outTerm, Range)))
adjoinChildren sources infos constructor children = wrap <$> leadingContext <> lines
  where (lines, next) = foldr (childLines sources) (mempty, end <$> ranges) children
        ranges = characterRange <$> infos
        categories = Diff.categories <$> infos
        leadingContext = tsequenceL (pure mempty) $ makeContextLines <$> (linesInRangeOfSource <$> (Range <$> (start <$> ranges) <*> next) <*> sources)
        wrap = (wrapLineContents <$> (makeBranchTerm constructor <$> categories <*> next) <*>)
        makeBranchTerm constructor categories next children = let range = unionRangesFrom (rangeAt next) $ Prelude.snd <$> children in
          (constructor (Info range categories) . catMaybes . toList $ Prelude.fst <$> children, range)

-- | Accumulate the lines of and between a branch term’s children.
childLines :: (Copointed c, Functor c, Applicative f, Coalescent (f (Line (Maybe (c a), Range))), Foldable f, TotalCrosswalk f) => f (Source Char) -> c (Adjoined (f (Line (a, Range)))) -> (Adjoined (f (Line (Maybe (c a), Range))), f Int) -> (Adjoined (f (Line (Maybe (c a), Range))), f Int)
-- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
childLines sources child (nextLines, next) | or ((>) . end <$> childRanges <*> next) = (nextLines, next)
                                           | otherwise = ((makeChildLines <$> copoint child)
                                                         <> tsequenceL (pure mempty) (makeContextLines <$> trailingContextLines)
                                                         <> nextLines, start <$> childRanges)
  where makeChildLines = fmap (fmap (first (Just . (<$ child))))
        trailingContextLines = linesInRangeOfSource <$> (Range <$> (end <$> childRanges) <*> next) <*> sources
        childRanges = unionRangesFrom <$> (rangeAt <$> next) <*> (concat . fmap (fmap Prelude.snd . unLine) <$> sequenceA (copoint child))

makeContextLines :: Adjoined (Line Range) -> Adjoined (Line (Maybe a, Range))
makeContextLines = fmap (fmap ((,) Nothing))

-- | Produce open/closed lines for the portion of the source spanned by a range.
linesInRangeOfSource :: Range -> Source Char -> Adjoined (Line Range)
linesInRangeOfSource range source = fromList $ pureBy (openRange source) <$> actualLineRanges range source

-- | Does this Range in this Source end with a newline?
openRange :: Source Char -> Range -> Bool
openRange source range = (at source <$> maybeLastIndex range) /= Just '\n'

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Both (Line a)
