{-# LANGUAGE RankNTypes #-}
module Alignment
( hasChanges
, numberedRows
, AlignedDiff
, alignDiff
, alignBranch
, applyThese
, modifyJoin
, unionThese
) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Data.Align
import Data.Biapplicative
import Data.Bifunctor.Join
import Data.Copointed
import Data.Foldable
import Data.Function
import Data.Functor.Both as Both
import Data.Functor.Foldable hiding (Foldable, fold)
import Data.Functor.Identity
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import qualified Data.OrderedMap as Map
import Data.These
import Diff
import Info
import Patch
import Prologue hiding (fst, snd)
import Range
import Source hiding (break, fromList, uncons, (++))
import SplitDiff
import Syntax
import Term

-- | Assign line numbers to the lines on each side of a list of rows.
numberedRows :: [Join These a] -> [Join These (Int, a)]
numberedRows = countUp (both 1 1)
  where countUp from (row : rows) = fromJust ((,) <$> modifyJoin (uncurry These) from `applyThese` row) : countUp (modifyJoin (fromThese identity identity) (succ <$ row) <*> from) rows
        countUp _ [] = []

-- | Determine whether a line contains any patches.
hasChanges :: SplitDiff leaf Info -> Bool
hasChanges = or . (True <$)

type AlignedDiff leaf = [Join These (SplitDiff leaf Info)]

alignDiff :: Both (Source Char) -> Diff leaf Info -> AlignedDiff leaf
alignDiff sources diff = iter (alignSyntax (runBothWith ((Join .) . These)) (free . Free) getRange sources) (alignPatch sources <$> diff)

alignPatch :: Both (Source Char) -> Patch (Term leaf Info) -> AlignedDiff leaf
alignPatch sources patch = case patch of
  Delete term -> fmap (pure . SplitDelete) <$> hylo (alignSyntax this cofree getRange (Identity (fst sources))) runCofree (Identity <$> term)
  Insert term -> fmap (pure . SplitInsert) <$> hylo (alignSyntax that cofree getRange (Identity (snd sources))) runCofree (Identity <$> term)
  Replace term1 term2 -> fmap (pure . SplitReplace) <$> alignWith (fmap (these identity identity const . runJoin) . Join)
    (hylo (alignSyntax this cofree getRange (Identity (fst sources))) runCofree (Identity <$> term1))
    (hylo (alignSyntax that cofree getRange (Identity (snd sources))) runCofree (Identity <$> term2))
  where getRange = characterRange . extract
        this = Join . This . runIdentity
        that = Join . That . runIdentity

-- | The Applicative instance f is either Identity or Both. Identity is for Terms in Patches, Both is for Diffs in unchanged portions of the diff.
alignSyntax :: Applicative f => (forall a. f a -> Join These a) -> (CofreeF (Syntax leaf) Info term -> term) -> (term -> Range) -> f (Source Char) -> CofreeF (Syntax leaf) (f Info) [Join These term] -> [Join These term]
alignSyntax toJoinThese toNode getRange sources (infos :< syntax) = case syntax of
  Leaf s -> catMaybes $ wrapInBranch (const (Leaf s)) . fmap (flip (,) []) <$> sequenceL lineRanges
  Indexed children -> catMaybes $ wrapInBranch (Indexed . fmap runIdentity) <$> alignBranch getRange (Identity <$> children) (modifyJoin (fromThese [] []) lineRanges)
  Fixed children -> catMaybes $ wrapInBranch (Fixed . fmap runIdentity) <$> alignBranch getRange (Identity <$> children) (modifyJoin (fromThese [] []) lineRanges)
  Keyed children -> catMaybes $ wrapInBranch (Keyed . Map.fromList) <$> alignBranch getRange (Map.toList children) (modifyJoin (fromThese [] []) lineRanges)
  where lineRanges = toJoinThese $ actualLineRanges <$> (characterRange <$> infos) <*> sources
        wrapInBranch constructor = applyThese $ toJoinThese ((\ info (range, children) -> toNode (info { characterRange = range } :< constructor children)) <$> infos)

{-


We align asymmetrically since the first child is asymmetrical, and then continue aligning symmetrically afterwards:
[         | [
  a       |
, b       |   b
]         | ]

The first child is asymmetrical but there is also a symmetrical child on the same line, so we align symmetrically, producing:
[ a, b ] | [ b ]

and not:
[ a, b ] |
         | [ b ]

We align the child symmetrically, and thus have to take the first line range on the right asymmetrically so as not to break the child’s alignment.
      | [
[ b ] |   b
      | ]
(Eventually, we’ll align the left hand side of this up a line, but that constraint is undecidable for now.)


If a is replaced with b in a Replace patch, we would like to align them side by side (that’s what makes it a replacement—they correlate), but a catamorphism which loses the Replace relationship (by splitting it into two SplitReplaces) can’t know that they’re related:
[ a ] | [ b ]

If a is deleted and b is coincidentally inserted, we want to separate them, because they’re semantically unrelated:
[ a ] |
      | [ b ]

The presence of a symmetrical child forces it to be symmetrical again:
[ a, c ] | [ c, b ]

We might split up children so `This` and `That` aren’t 1:1 with `Delete` and `Insert`. This is because earlier symmetrical children take precedence over later ones:
[ a, b ] | [ a
         | , b
         | ]

Lines without children on them are aligned irrespective of their textual content:
[\n      | [\n
  a\n    |   a, b\n
,\n      | \n
  b\n    | \n
]        | ]

We should avoid taking asymmetrical children greedily so as not to misalign asymmetrical children before symmetrical children on the same line:
         | [ a
[ b, c ] | , c
         | ]

-}

-- | Given a function to get the range, a list of already-aligned children, and the lists of ranges spanned by a branch, return the aligned lines.
alignBranch :: (Copointed c, Functor c) => (term -> Range) -> [c [Join These term]] -> Both [Range] -> [Join These (Range, [c term])]
-- The first child is empty, and so can safely be dropped.
alignBranch getRange (first:children) ranges | null (copoint first) = alignBranch getRange children ranges
-- There are no more ranges, so we’re done.
alignBranch _ [] (Join ([], [])) = []
alignBranch _ children (Join ([], [])) = trace ("exhausted ranges with " ++ show (length children) ++ " children remaining") []
-- There are no more children, so we can just zip the remaining ranges together.
alignBranch _ [] ranges = runBothWith (alignWith Join) (fmap (flip (,) []) <$> ranges)
-- There are both children and ranges, so we need to proceed line by line
alignBranch getRange children ranges = case intersectingChildren of
  -- No child intersects the current ranges on either side, so advance.
  [] -> (flip (,) [] <$> headRanges) : alignBranch getRange children (drop 1 <$> ranges)
  -- At least one child intersects on at least one side.
  _ -> case fromThese True True . runJoin . intersectsFirstLine getRange headRanges . copoint <$> listToMaybe remainingIntersectingChildren of
    -- At least one child intersects on both sides, so align symmetrically.
    Just (True, True) -> let (line, remaining) = lineAndRemaining intersectingChildren headRanges in
      line : alignBranch getRange (remaining ++ nonIntersectingChildren) (drop 1 <$> ranges)
    -- A symmetrical child intersects on the right, so align asymmetrically on the left.
    Just (False, True) -> let (leftLine, remainingAtLeft) = maybe (id, []) (first (:)) $ lineAndRemaining asymmetricalChildren <$> leftRange in
      leftLine $ alignBranch getRange (remainingAtLeft ++ remainingIntersectingChildren ++ nonIntersectingChildren) (modifyJoin (first (drop 1)) ranges)
    -- A symmetrical child intersects on the left, so align asymmetrically on the right.
    Just (True, False) -> let (rightLine, remainingAtRight) = maybe (id, []) (first (:)) $ lineAndRemaining asymmetricalChildren <$> rightRange in
      rightLine $ alignBranch getRange (remainingAtRight ++ remainingIntersectingChildren ++ nonIntersectingChildren) (modifyJoin (second (drop 1)) ranges)
    -- No symmetrical child intersects, so align asymmetrically, picking the left side first to match the deletion/insertion order convention in diffs.
    _ -> let (leftLine, remainingAtLeft) = maybe (identity, []) (first (:)) $ leftRange >>= lineAndRemainingWhere (maybe False (isThis . runJoin) . head . copoint) asymmetricalChildren
             (rightLine, remainingAtRight) = maybe (identity, []) (first (:)) $ rightRange >>= lineAndRemainingWhere (maybe False (isThat . runJoin) . head . copoint) asymmetricalChildren in
      leftLine $ rightLine $ alignBranch getRange (remainingAtLeft ++ remainingAtRight ++ nonIntersectingChildren) (modifyJoin (uncurry bimap (advancePast (fromJust . head . copoint <$> asymmetricalChildren))) ranges)
  where (intersectingChildren, nonIntersectingChildren) = partition (or . intersectsFirstLine getRange headRanges . copoint) children
        (remainingIntersectingChildren, asymmetricalChildren) = partition (maybe False (isThese . runJoin) . head . copoint) intersectingChildren
        Just headRanges = headRangesOf ranges
        (leftRange, rightRange) = splitThese headRanges
        lineAndRemaining children ranges = let (intersections, remaining) = alignChildren getRange children ranges in
          ((,) <$> ranges `applyToBoth` intersections, remaining)
        lineAndRemainingWhere predicate children = if any predicate children then Just . lineAndRemaining (filter predicate children) else const Nothing

advancePast :: [Join These term] -> ([a] -> [a], [a] -> [a])
advancePast children = fromThese identity identity . runJoin . (drop 1 <$) $ unionThese children

headRangesOf :: Both [Range] -> Maybe (Join These Range)
headRangesOf ranges = sequenceL (listToMaybe <$> Join (runBothWith These ranges))

intersectsFirstLine :: (term -> Range) -> Join These Range -> [Join These term] -> Join These Bool
intersectsFirstLine getRange ranges = maybe (False <$ ranges) (intersects getRange ranges) . listToMaybe

intersectsAnyLine :: (term -> Range) -> Join These Range -> [Join These term] -> Join These Bool
intersectsAnyLine getRange ranges = foldr (orIntersects ranges) (False <$ ranges)
  where orIntersects ranges line next = fromMaybe (False <$ ranges) ((||) <$> intersects getRange ranges line `applyThese` next)

-- | Given a list of aligned children, produce lists of their intersecting first lines, and a list of the remaining lines/nonintersecting first lines.
alignChildren :: (Copointed c, Functor c) => (term -> Range) -> [c [Join These term]] -> Join These Range -> (Both [c term], [c [Join These term]])
alignChildren _ [] _ = (both [] [], [])
alignChildren getRange (first:rest) headRanges
  | (firstLine:restOfLines) <- copoint first
  , ~(l, r) <- splitThese firstLine
  = case fromThese False False . runJoin $ intersectsFirstLine getRange headRanges (copoint first) of
  -- It intersects on both sides, so we can just take the first line whole.
  (True, True) -> ((++) <$> toTerms firstLine <*> firstRemaining, (restOfLines <$ first) : restRemaining)
  -- It only intersects on the left, so split it up.
  (True, False) -> ((++) <$> toTerms (fromJust l) <*> firstRemaining, (maybe identity (:) r restOfLines <$ first) : restRemaining)
  -- It only intersects on the right, so split it up.
  (False, True) -> ((++) <$> toTerms (fromJust r) <*> firstRemaining, (maybe identity (:) l restOfLines <$ first) : restRemaining)
  -- It doesn’t intersect at all, so skip it and move along.
  (False, False) -> (firstRemaining, first:restRemaining)
  | otherwise = alignChildren getRange rest headRanges
  where (firstRemaining, restRemaining) = alignChildren getRange rest headRanges
        toTerms line = modifyJoin (fromThese [] []) (pure . (<$ first) <$> line)

unionThese :: (Alternative f, Foldable f, Monoid (f a)) => f (Join These a) -> Join These (f a)
unionThese as = fromMaybe (Join (These empty empty)) . getUnion . fold $ Union . Just . fmap pure <$> as

-- | Test ranges and terms for intersection on either or both sides.
intersects :: (term -> Range) -> Join These Range -> Join These term -> Join These Bool
intersects getRange ranges line = fromMaybe (False <$ ranges) $ intersectsRange <$> ranges `applyThese` modifyJoin (uncurry These . fromThese (Range (-1) (-1)) (Range (-1) (-1))) (getRange <$> line)

-- | Split a These value up into independent These values representing the left and right sides, if any.
splitThese :: Join These a -> (Maybe (Join These a), Maybe (Join These a))
splitThese these = fromThese Nothing Nothing $ bimap (Just . Join . This) (Just . Join . That) (runJoin these)

infixl 4 `applyThese`

-- | Like `<*>`, but it returns its result in `Maybe` since the result is the intersection of the shapes of the inputs.
applyThese :: Join These (a -> b) -> Join These a -> Maybe (Join These b)
applyThese (Join fg) (Join ab) = fmap Join . uncurry maybeThese $ uncurry (***) (bimap (<*>) (<*>) (unpack fg)) (unpack ab)
  where unpack = fromThese Nothing Nothing . bimap Just Just

infixl 4 `applyToBoth`

-- | Like `<*>`, but it takes a `Both` on the right to ensure that it can always return a value.
applyToBoth :: Join These (a -> b) -> Both a -> Join These b
applyToBoth (Join fg) (Join (a, b)) = Join $ these (This . ($ a)) (That . ($ b)) (\ f g -> These (f a) (g b)) fg

-- Map over the bifunctor inside a Join, producing another Join.
modifyJoin :: (p a a -> q b b) -> Join p a -> Join q b
modifyJoin f = Join . f . runJoin

-- | Given a pair of Maybes, produce a These containing Just their values, or Nothing if they haven’t any.
maybeThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeThese (Just a) (Just b) = Just (These a b)
maybeThese (Just a) _ = Just (This a)
maybeThese _ (Just b) = Just (That b)
maybeThese _ _ = Nothing

-- | A Monoid wrapping Join These, for which mappend is the smallest shape covering both arguments.
newtype Union a = Union { getUnion :: Maybe (Join These a) }
  deriving (Eq, Functor, Show)


-- | Instances

instance Monoid a => Monoid (Union a) where
  mempty = Union Nothing
  Union (Just a) `mappend` Union (Just b) = Union $ Join <$> uncurry maybeThese (uncurry (***) (bimap mappend mappend (unpack a)) (unpack b))
    where unpack = fromThese Nothing Nothing . runJoin . fmap Just
  Union (Just a) `mappend` _ = Union $ Just a
  Union _ `mappend` Union (Just b) = Union $ Just b
  _ `mappend` _ = Union Nothing

instance Bicrosswalk t => Crosswalk (Join t) where
  crosswalk f = fmap Join . bicrosswalk f f . runJoin
