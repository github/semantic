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

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Adjoined
import Data.Align
import Data.Bifunctor
import Data.Bifunctor.Join
import Data.Bifunctor.These
import Data.Coalescent
import Data.Copointed
import Data.Foldable
import Data.Functor.Both as Both hiding (unzip)
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
  Leaf a -> tsequenceL (pure mempty) $ fmap <$> ((\ categories -> fmap (\ range -> (makeTerm (Info range categories) (Leaf a), range))) <$> (Info.categories <$> infos)) <*> (linesInRangeOfSource <$> (characterRange <$> infos) <*> sources)
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
        childRanges = unionRangesFrom <$> (rangeAt <$> next) <*> (concatMap (fmap Prelude.snd . unLine) <$> sequenceA (copoint child))

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

type AlignedDiff leaf = [Join These (SplitDiff leaf Info)]

alignPatch :: Both (Source Char) -> Patch (Term leaf Info) -> AlignedDiff leaf
alignPatch _ _ = []
-- alignPatch sources patch = crosswalk (hylo (alignTerm sources) unCofree) (unPatch patch)
-- alignPatch sources (Insert term) = hylo (alignTerm sources) unCofree term
-- alignPatch sources (Delete term) = hylo (alignTerm sources) unCofree term
-- alignPatch sources (Replace term1 term2) = alignWith Join (hylo (alignTerm sources) unCofree term1)
--                                                           (hylo (alignTerm sources) unCofree term2)
--
-- alignTerm :: Both (Source Char) -> Join These Info -> Syntax leaf (AlignedDiff leaf) -> AlignedDiff leaf
-- alignTerm sources infos syntax = (\ (source, info) -> Free . Annotated info <$> alignSyntax source (characterRange info) syntax) <$> Join (pairWithThese sources (runJoin infos))

alignDiff :: Both (Source Char) -> Diff leaf Info -> AlignedDiff leaf
alignDiff sources diff = iter alignSyntax (alignPatch sources <$> diff)
  where alignSyntax :: Annotated leaf (Both Info) (AlignedDiff leaf) -> AlignedDiff leaf
        alignSyntax (Annotated infos syntax) = case syntax of
          Leaf s -> modifyJoin (runBothWith bimap (((Free . (`Annotated` Leaf s)) .) . setCharacterRange <$> infos)) <$> sequenceL lineRanges
          Indexed children -> modifyJoin (runBothWith bimap ((\ info (range, children) -> Free (Annotated (setCharacterRange info range) (Indexed children))) <$> infos)) <$> groupChildrenByLine lineRanges children
          _ -> []
          where lineRanges = runBothWith ((Join .) . These) (actualLineRanges <$> (characterRange <$> infos) <*> sources)

groupChildrenByLine :: Join These [Range] -> [AlignedDiff leaf] -> [Join These (Range, [SplitDiff leaf Info])]
groupChildrenByLine ranges children | Just (headRanges, tailRanges) <- unconsThese ranges
                                    , (intersectingChildren, rest) <- spanMergeable headRanges children
                                    , ~(intersectingChildrenL, intersectingChildrenR) <- bimap catMaybes catMaybes (unalign $ runJoin <$> join intersectingChildren)
                                    = (case runJoin headRanges of
                                        This l -> Join $ This (l, intersectingChildrenL)
                                        That r -> Join $ That (r, intersectingChildrenR)
                                        These l r -> Join $ These (l, intersectingChildrenL) (r, intersectingChildrenR))
                                          : groupChildrenByLine tailRanges rest
                                    | otherwise = []

group2 :: Join These [Range] -> AlignedDiff leaf -> (Join These [Range], [Join These (SplitDiff leaf Info)])
group2 ranges child | Just (headRanges, tailRanges) <- unconsThese ranges
                    , Just rrrraaaangggeessss <- sequenceL $ uncons <$> ranges
                    , (first:rest) <- child
                    = case fromThese False False . runJoin $ intersects headRanges child of
                        (True, True) -> let (moreRanges, restOfChild) = group2 tailRanges rest in
                                          (moreRanges, first : restOfChild)
                        (True, False) -> let (moreRanges, restOfChild) = group2 (atLeft ranges) rest in
                                           (moreRanges, first : restOfChild)
                        (False, True) -> let (moreRanges, restOfChild) = group2 (atRight ranges) rest in
                                           (moreRanges, first : restOfChild)
                        _ -> (tailRanges, [])
                    | otherwise = (ranges, [])
                    where uncons :: [a] -> Maybe (a, [a])
                          uncons (a:as) = Just (a, as)
                          uncons _ = Nothing
                          atLeft (Join (These (a:as) bs)) = Join (These as bs)
                          atLeft (Join (This (a:as))) = Join (This as)
                          atLeft other = other
                          atRight (Join (These as (b:bs))) = Join (These as bs)
                          atRight (Join (That (b:bs))) = Join (That bs)
                          atRight other = other

{-

find all of the lines which intersect with this child, not all the children intersecting with this line?

[ foo ]   [
          foo
          ]


[ Join These "[ foo ]" "[\n"]
[ Join That            "foo\n"]
[ Join That            "]"]

[ Range 2 5 ] [ Range 2 5 ]
[ Range 0 7 ] [ Range 0 2, Range 2 6, Range 6 7 ]



[ foo ]
bar

[
foo
]
bar

[ foo ]   [
          foo
          ]
bar       bar


[ Range 0 8, Range 8 12 ]  [ Range 0 2, Range 2 6, Range 6 8, Range 8 12 ]

[ foo ]   [
bar       foo
          ]
          bar

1. we need to consume lines until the child’s lines have been exhausted


2. (AND all the children intersecting the line have been consumed)


-[ foo ]
+[
+foo
+]
 bar

-}

maybeThese :: (Maybe a, Maybe b) -> Maybe (These a b)
maybeThese (Just a, Just b) = Just (These a b)
maybeThese (Just a, _) = Just (This a)
maybeThese (_, Just b) = Just (That b)
maybeThese _ = Nothing

unconsThese :: Join These [a] -> Maybe (Join These a, Join These [a])
unconsThese (Join (This (a:as))) = Just (Join (This a), Join (This as))
unconsThese (Join (That (b:bs))) = Just (Join (That b), Join (That bs))
unconsThese (Join (These (a:as) (b:bs))) = Just (Join (These a b), Join (These as bs))
unconsThese (Join (These (a:as) _)) = Just (Join (This a), Join (This as))
unconsThese (Join (These _ (b:bs))) = Just (Join (That b), Join (That bs))
unconsThese _ = Nothing

getRange :: SplitDiff leaf Info -> Range
getRange (Free (Annotated (Info range _) _)) = range
getRange (Pure patch) | Info range _ :< _ <- getSplitTerm patch = range

intersects :: Join These Range -> AlignedDiff leaf -> Join These Bool
intersects ranges childLines | (line:_) <- childLines = fromMaybe (False <$ line) $ intersectsChild <$> ranges `applyThese` line
                             | otherwise = False <$ ranges

intersectsChild :: Range -> SplitDiff leaf Info -> Bool
intersectsChild range child = end (getRange child) <= end range

spanMergeable :: Join These Range -> [AlignedDiff leaf] -> ([AlignedDiff leaf], [AlignedDiff leaf])
spanMergeable ranges children | (child:rest) <- children
                              , ~(merge, nope) <- spanMergeable ranges rest
                              , ~(this, that) <- unzip $ split <$> child
                              = case fromThese False False . runJoin $ intersects ranges child of
                                  (True, True) -> (child:merge, nope)
                                  (True, False) -> (this ++ merge, that ++ nope)
                                  (False, True) -> (that ++ merge, this ++ nope)
                                  _ -> ([], children)
                              | otherwise = ([], [])

split :: Join These a -> ([Join These a], [Join These a])
split these = fromThese [] [] $ bimap (pure . Join . This) (pure . Join . That) (runJoin these)

infixl 4 `applyThese`

applyThese :: Join These (a -> b) -> Join These a -> Maybe (Join These b)
applyThese (Join (This f)) (Join (This a)) = Just (Join (This (f a)))
applyThese (Join (That g)) (Join (That b)) = Just (Join (That (g b)))
applyThese (Join (These f g)) (Join (These a b)) = Just (Join (These (f a) (g b)))
applyThese (Join (These f _)) (Join (This a)) = Just (Join (This (f a)))
applyThese (Join (These _ g)) (Join (That b)) = Just (Join (That (g b)))
applyThese (Join (This f)) (Join (These a _)) = Just (Join (This (f a)))
applyThese (Join (That g)) (Join (These _ b)) = Just (Join (That (g b)))
applyThese _ _ = Nothing

modifyJoin :: (p a a -> q b b) -> Join p a -> Join q b
modifyJoin f = Join . f . runJoin

-- alignSyntax :: Source Char -> Range -> Syntax leaf (AlignedDiff leaf) -> [Syntax leaf (SplitDiff leaf Info)]
-- alignSyntax source range syntax = case syntax of
--   Leaf s -> Leaf s <$ lineRanges
--   _ -> []
--   where lineRanges = actualLineRanges range source
