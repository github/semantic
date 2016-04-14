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
import Data.Bifunctor
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
alignPatch sources (Delete term) = hylo (alignSyntax (Join . This . runIdentity) (Identity (fst sources))) unCofree (Identity <$> term)
alignPatch sources (Insert term) = hylo (alignSyntax (Join . That . runIdentity) (Identity (snd sources))) unCofree (Identity <$> term)
alignPatch sources (Replace term1 term2) = alignWith (fmap (these id id const . runJoin) . Join)
                                            (hylo (alignSyntax (Join . This . runIdentity) (Identity (fst sources))) unCofree (Identity <$> term1))
                                            (hylo (alignSyntax (Join . That . runIdentity) (Identity (snd sources))) unCofree (Identity <$> term2))

alignDiff :: Both (Source Char) -> Diff leaf Info -> AlignedDiff leaf
alignDiff sources diff = iter (uncurry (alignSyntax (runBothWith ((Join .) . These)) sources) . (annotation &&& syntax)) (alignPatch sources <$> diff)

alignSyntax :: Applicative f => (forall a. f a -> Join These a) -> f (Source Char) -> f Info -> Syntax leaf (AlignedDiff leaf) -> AlignedDiff leaf
alignSyntax toJoinThese sources infos syntax = case syntax of
  Leaf s -> catMaybes $ wrapInBranch (const (Leaf s)) . fmap (flip (,) []) <$> sequenceL lineRanges
  Indexed children -> catMaybes $ wrapInBranch Indexed <$> groupChildrenByLine lineRanges children
  Fixed children -> catMaybes $ wrapInBranch Fixed <$> groupChildrenByLine lineRanges children
  _ -> []
  where lineRanges = toJoinThese $ actualLineRanges <$> (characterRange <$> infos) <*> sources
        wrapInBranch constructor = applyThese $ toJoinThese ((\ info (range, children) -> Free (Annotated (setCharacterRange info range) (constructor children))) <$> infos)

groupChildrenByLine :: Join These [Range] -> [AlignedDiff leaf] -> [Join These (Range, [SplitDiff leaf Info])]
groupChildrenByLine ranges children | not (and $ null <$> ranges)
                                    , (nextRanges, nextChildren, lines) <- group2 ranges children
                                    = lines ++ groupChildrenByLine nextRanges nextChildren
                                    | otherwise = []

group2 :: Join These [Range] -> [AlignedDiff leaf] -> (Join These [Range], [AlignedDiff leaf], [Join These (Range, [SplitDiff leaf Info])])
group2 ranges children | Just headRanges <- sequenceL $ listToMaybe <$> ranges
                       , (intersecting, nonintersecting) <- spanAndSplitFirstLines (intersects headRanges) children
                       , (thisLine, nextLines) <- foldr (\ (this, next) (these, nexts) -> (this : these, next ++ nexts)) ([], []) intersecting
                       , merged <- pairRangesWithLine headRanges . mask headRanges . fmap join . Join . uncurry These . unzip $ fromThese [] [] . runJoin . fmap pure <$> thisLine
                       , fs <- fromThese id id . runJoin . fmap (const (drop 1)) <$> listToMaybe nextLines
                       , (nextRanges, nextChildren, nextLines) <- group2 (modifyJoin (uncurry bimap $ fromMaybe (drop 1, drop 1) fs) ranges) (nextLines : nonintersecting)
                       = (nextRanges, nextChildren, merged : nextLines)
                       | ([]:rest) <- children = group2 ranges rest
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

mergeThese :: [Join These a] -> Join These [a]
mergeThese [ a ] = pure <$> a
mergeThese (a:as) = fromMaybe (Join (These [] [])) $ (:) <$> a `applyThese` mergeThese as
mergeThese [] = Join (These [] [])

-- | Partitions and splits a list of children into a tuple consisting of:
-- | - elements which matched; if an element matches only partially this field will contain only the matching side
-- | - the left sides of elements which matched only on the right side
-- | - the right sides of elements which matched only on the left side
-- | - elements which do not intersect.
spanThese :: (Join These a -> Join These Bool) -> [[Join These a]] -> ([[Join These a]], [[Join These a]], [[Join These a]], [[Join These a]])
spanThese pred children | (child:rest) <- children
                        , not (null child)
                        , ~(moreChildren, moreL, moreR, moreLines) <- spanThese pred rest
                        , ~(l, r) <- split (head child)
                        = case fromThese False False (runJoin (pred (head child))) of
                          (True, True) -> (child : moreChildren, moreL, moreR, moreLines)
                          (True, False) -> (l : moreChildren, moreL, r : moreR, moreLines)
                          (False, True) -> (r : moreChildren, l : moreL, moreR, moreLines)
                          _ -> ([], [], [], children)
                        | ([]:rest) <- children = spanThese pred rest
                        | otherwise = ([], [], [], children)

pairRangesWithLine :: Monoid b => Join These a -> Join These b -> Join These (a, b)
pairRangesWithLine headRanges childLine = fromMaybe (flip (,) mempty <$> headRanges) $ (,) <$> headRanges `applyThese` childLine

mask :: Join These a -> Join These b -> Join These b
mask (Join (This _)) (Join (This b1)) = Join $ This b1
mask (Join (This _)) (Join (These b1 _)) = Join $ This b1
mask (Join (That _)) (Join (That b2)) = Join $ That b2
mask (Join (That _)) (Join (These _ b2)) = Join $ That b2
mask (Join (These _ _)) (Join (This b1)) = Join $ This b1
mask (Join (These _ _)) (Join (That b2)) = Join $ That b2
mask (Join (These _ _)) (Join (These b1 b2)) = Join $ These b1 b2
mask _ b = b

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

intersects :: Join These Range -> Join These (SplitDiff leaf Info) -> Join These Bool
intersects ranges line = fromMaybe (False <$ line) $ intersectsChild <$> ranges `applyThese` line

intersectsChild :: Range -> SplitDiff leaf Info -> Bool
intersectsChild range child = end (getRange child) <= end range

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
