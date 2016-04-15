{-# LANGUAGE RankNTypes #-}
module Alignment
( hasChanges
, linesInRangeOfSource
, numberedRows
, splitAbstractedTerm
, splitDiffByLines
, Row
, AlignedDiff
, alignDiff
, groupChildrenByLine
) where

import Control.Arrow ((&&&))
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Adjoined
import Data.Align
import Data.Biapplicative
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
import qualified Data.Text as T
import Diff
import Info
import Line
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import Source hiding (fromList, uncons, (++))
import SplitDiff
import Syntax
import Term

-- | Assign line numbers to the lines on each side of a list of rows.
numberedRows :: [Join These a] -> [Join These (Int, a)]
numberedRows = countUp (Join $ These 1 1)
  where countUp from (row : rows) = fromJust ((,) <$> from `applyThese` row) : countUp (succ <$> from) rows
        countUp _ [] = []

-- | Determine whether a line contains any patches.

-- | Split a diff, which may span multiple lines, into rows of split diffs paired with the Range of characters spanned by that Row on each side of the diff.
splitDiffByLines :: Both (Source Char) -> Diff leaf Info -> [Row (SplitDiff leaf Info, Range)]
splitDiffByLines sources = toList . iter (\ (Annotated infos syntax) -> splitAbstractedTerm ((Free .) . Annotated) sources infos syntax) . fmap (splitPatchByLines sources)

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Both (Source Char) -> Patch (Term leaf Info) -> Adjoined (Both (Line (SplitDiff leaf Info, Range)))
splitPatchByLines sources patch = wrapTermInPatch <$> splitAndFoldTerm (unPatch patch)
    where splitAndFoldTerm (This deleted) = tsequenceL mempty $ both (runIdentity <$> cata (splitAbstractedTerm (:<) (Identity $ fst sources)) (Identity <$> deleted)) nil
          splitAndFoldTerm (That inserted) = tsequenceL mempty $ both nil (runIdentity <$> cata (splitAbstractedTerm (:<) (Identity $ snd sources)) (Identity <$> inserted))
          splitAndFoldTerm (These deleted inserted) = tsequenceL mempty $ both (runIdentity <$> cata (splitAbstractedTerm (:<) (Identity $ fst sources)) (Identity <$> deleted)) (runIdentity <$> cata (splitAbstractedTerm (:<) (Identity $ snd sources)) (Identity <$> inserted))
          wrapTermInPatch = fmap (fmap (first (Pure . constructor patch)))
          constructor (Replace _ _) = SplitReplace
          constructor (Insert _) = SplitInsert
          constructor (Delete _) = SplitDelete

-- | Split a term comprised of an Info & Syntax up into one `outTerm` (abstracted by an alignment function & constructor) per line in `Source`.
splitAbstractedTerm :: (Applicative f, Coalescent (f (Line (Maybe (Identity outTerm), Range))), Coalescent (f (Line (Maybe (T.Text, outTerm), Range))), Foldable f, TotalCrosswalk f) => (Info -> Syntax leaf outTerm -> outTerm) -> f (Source Char) -> f Info -> Syntax leaf (Adjoined (f (Line (outTerm, Range)))) -> Adjoined (f (Line (outTerm, Range)))
splitAbstractedTerm makeTerm sources infos syntax = case syntax of
  Leaf a -> let lineRanges = linesInRangeOfSource <$> (characterRange <$> infos) <*> sources in
    tsequenceL (pure mempty)
      $ fmap <$> ((\ info -> fmap (\ range -> (makeTerm info { characterRange = range } (Leaf a), range))) <$> infos) <*> lineRanges
  Indexed children -> adjoinChildren sources infos (constructor (Indexed . fmap runIdentity)) (Identity <$> children)
  Fixed children -> adjoinChildren sources infos (constructor (Fixed . fmap runIdentity)) (Identity <$> children)
  Keyed children -> adjoinChildren sources infos (constructor (Keyed . Map.fromList)) (Map.toList children)
  where constructor with info = makeTerm info . with

-- | Adjoin a branch term’s lines, wrapping children & context in branch nodes using a constructor.
adjoinChildren :: (Copointed c, Functor c, Applicative f, Coalescent (f (Line (Maybe (c a), Range))), Foldable f, TotalCrosswalk f) => f (Source Char) -> f Info -> (Info -> [c a] -> outTerm) -> [c (Adjoined (f (Line (a, Range))))] -> Adjoined (f (Line (outTerm, Range)))
adjoinChildren sources infos constructor children = wrap <$> leadingContext <> lines
  where (lines, next) = foldr (childLines sources) (mempty, end <$> ranges) children
        ranges = characterRange <$> infos
        categories = Info.categories <$> infos
        sizes = size <$> infos
        leadingContext = tsequenceL (pure mempty) $ makeContextLines <$> (linesInRangeOfSource <$> (Range <$> (start <$> ranges) <*> next) <*> sources)
        wrap = (wrapLineContents <$> (makeBranchTerm constructor <$> categories <*> sizes <*> next) <*>)
        makeBranchTerm constructor categories size next children = let range = unionRangesFrom (rangeAt next) $ Prelude.snd <$> children in
          (constructor (Info range categories size) . catMaybes . toList $ Prelude.fst <$> children, range)

-- | Accumulate the lines of and between a branch term’s children.
childLines :: (Copointed c, Functor c, Applicative f, Coalescent (f (Line (Maybe (c a), Range))), Foldable f, TotalCrosswalk f) => f (Source Char) -> c (Adjoined (f (Line (a, Range)))) -> (Adjoined (f (Line (Maybe (c a), Range))), f Int) -> (Adjoined (f (Line (Maybe (c a), Range))), f Int)
-- We depend on source ranges increasing monotonically. If a child invalidates that, e.g. if it’s a move in a Keyed node, we don’t output rows for it in this iteration. (It will still show up in the diff as context rows.) This works around https://github.com/github/semantic-diff/issues/488.
childLines sources child (nextLines, next) | or ((>) . end <$> childRanges <*> next) = (nextLines, next)
                                           | otherwise = ((makeChildLines <$> copoint child)
                                                         <> tsequenceL (pure mempty) (makeContextLines <$> trailingContextLines)
                                                         <> nextLines, start <$> childRanges)
  where makeChildLines = fmap (fmap (first (Just . (<$ child))))
        trailingContextLines = linesInRangeOfSource <$> (Range <$> (end <$> childRanges) <*> next) <*> sources
        childRanges = unionRangesFrom <$> (rangeAt <$> next) <*> (concatMap (fmap Prelude.snd . unLine) <$> sequenceA (copoint child))

makeContextLines :: Adjoined (Line Range) -> Adjoined (Line (Maybe a, Range))
makeContextLines = fmap (fmap ((,) Nothing))

-- | Produce open/closed lines for the portion of the source spanned by a range.
linesInRangeOfSource :: Range -> Source Char -> Adjoined (Line Range)
linesInRangeOfSource range source = fromList $ pureBy (openRange source) <$> actualLineRanges range source

-- | Does this Range in this Source end with a newline?
openRange :: Source Char -> Range -> Bool
openRange source range = (at source <$> maybeLastIndex range) /= Just '\n'
hasChanges :: SplitDiff leaf Info -> Bool
hasChanges = or . (True <$)

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Join These a

type AlignedDiff leaf = [Join These (SplitDiff leaf Info)]

alignDiff :: Both (Source Char) -> Diff leaf Info -> AlignedDiff leaf
alignDiff sources diff = iter (uncurry (alignSyntax (runBothWith ((Join .) . These)) ((Free .) . Annotated) getRange sources) . (annotation &&& syntax)) (alignPatch sources <$> diff)

alignPatch :: Both (Source Char) -> Patch (Term leaf Info) -> AlignedDiff leaf
alignPatch sources patch = case patch of
  Delete term -> fmap (Pure . SplitDelete) <$> hylo (alignSyntax (Join . This . runIdentity) (:<) getRange (Identity (fst sources))) unCofree (Identity <$> term)
  Insert term -> fmap (Pure . SplitInsert) <$> hylo (alignSyntax (Join . That . runIdentity) (:<) getRange (Identity (snd sources))) unCofree (Identity <$> term)
  Replace term1 term2 -> fmap (Pure . SplitReplace) <$> alignWith (fmap (these id id const . runJoin) . Join)
    (hylo (alignSyntax this (:<) getRange (Identity (fst sources))) unCofree (Identity <$> term1))
    (hylo (alignSyntax that (:<) getRange (Identity (snd sources))) unCofree (Identity <$> term2))
  where getRange = characterRange . copoint
        this = Join . This . runIdentity
        that = Join . That . runIdentity

alignSyntax :: Applicative f => (forall a. f a -> Join These a) -> (Info -> Syntax leaf term -> term) -> (term -> Range) -> f (Source Char) -> f Info -> Syntax leaf [Join These term] -> [Join These term]
alignSyntax toJoinThese toNode getRange sources infos syntax = case syntax of
  Leaf s -> catMaybes $ wrapInBranch (const (Leaf s)) . fmap (flip (,) []) <$> sequenceL lineRanges
  Indexed children -> catMaybes $ wrapInBranch Indexed <$> groupChildrenByLine getRange lineRanges children
  Fixed children -> catMaybes $ wrapInBranch Fixed <$> groupChildrenByLine getRange lineRanges children
  Keyed children -> catMaybes $ wrapInBranch Fixed <$> groupChildrenByLine getRange lineRanges (toList children)
  where lineRanges = toJoinThese $ actualLineRanges <$> (characterRange <$> infos) <*> sources
        wrapInBranch constructor = applyThese $ toJoinThese ((\ info (range, children) -> toNode (info { characterRange = range }) (constructor children)) <$> infos)

groupChildrenByLine :: (term -> Range) -> Join These [Range] -> [[Join These term]] -> [Join These (Range, [term])]
groupChildrenByLine getRange ranges children | not (and $ null <$> ranges)
                                             , (nextRanges, nextChildren, lines) <- alignChildrenInRanges getRange ranges children
                                             = lines ++ groupChildrenByLine getRange nextRanges nextChildren
                                             | otherwise = []

alignChildrenInRanges :: (term -> Range) -> Join These [Range] -> [[Join These term]] -> (Join These [Range], [[Join These term]], [Join These (Range, [term])])
alignChildrenInRanges getRange ranges children
  | Just headRanges <- sequenceL $ listToMaybe <$> ranges
  , (intersecting, nonintersecting) <- spanAndSplitFirstLines (intersects getRange headRanges) children
  , (thisLine, nextLines) <- foldr (\ (this, next) (these, nexts) -> (this : these, next ++ nexts)) ([], []) intersecting
  , thisRanges <- fromMaybe headRanges $ const <$> headRanges `applyThese` catThese (thisLine ++ nextLines)
  , merged <- pairRangesWithLine thisRanges (modifyJoin (uncurry These . fromThese [] []) (catThese thisLine))
  , advance <- fromMaybe (drop 1, drop 1) $ fromThese id id . runJoin . (drop 1 <$) <$> listToMaybe nextLines
  , (nextRanges, nextChildren, nextLines) <- alignChildrenInRanges getRange (modifyJoin (uncurry bimap advance) ranges) (nextLines : nonintersecting)
  = (nextRanges, nextChildren, merged : nextLines)
  | ([]:rest) <- children = alignChildrenInRanges getRange ranges rest
  | otherwise = ([] <$ ranges, children, fmap (flip (,) []) <$> sequenceL ranges)

spanAndSplitFirstLines :: (Join These a -> Join These Bool) -> [[Join These a]] -> ([(Join These a, [Join These a])], [[Join These a]])
spanAndSplitFirstLines pred = foldr go ([], [])
  where go child (intersecting, nonintersecting)
          | (first : rest) <- child = let ~(l, r) = split first in
            case fromThese False False . runJoin $ pred first of
              (True, True) -> ((first, rest) : intersecting, nonintersecting)
              (True, False) -> ((head l, r ++ rest) : intersecting, nonintersecting)
              (False, True) -> ((head r, l ++ rest) : intersecting, nonintersecting)
              _ -> (intersecting, (first : rest) : nonintersecting)
          | otherwise = (intersecting, nonintersecting)

catThese :: [Join These a] -> Join These [a]
catThese as = maybe (Join (These [] [])) Join $ getUnion $ mconcat $ Union . Just . runJoin . fmap pure <$> as

pairRangesWithLine :: Monoid b => Join These a -> Join These b -> Join These (a, b)
pairRangesWithLine headRanges childLine = fromMaybe (flip (,) mempty <$> headRanges) $ (,) <$> headRanges `applyThese` childLine

intersects :: (term -> Range) -> Join These Range -> Join These term -> Join These Bool
intersects getRange ranges line = fromMaybe (False <$ line) $ intersectsRange <$> ranges `applyThese` (getRange <$> line)

intersectsRange :: Range -> Range -> Bool
intersectsRange range1 range2 = end range2 <= end range1

split :: Join These a -> ([Join These a], [Join These a])
split these = fromThese [] [] $ bimap (pure . Join . This) (pure . Join . That) (runJoin these)

infixl 4 `applyThese`

applyThese :: Join These (a -> b) -> Join These a -> Maybe (Join These b)
applyThese fg ab = Join <$> runJoin fg `apThese` runJoin ab

modifyJoin :: (p a a -> q b b) -> Join p a -> Join q b
modifyJoin f = Join . f . runJoin
