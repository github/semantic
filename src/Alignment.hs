{-# LANGUAGE RankNTypes #-}
module Alignment
( hasChanges
, numberedRows
, AlignedDiff
, alignDiff
, alignChildrenInRanges
, applyThese
, modifyJoin
) where

import Control.Applicative
import Control.Arrow ((&&&), (***))
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Align
import Data.Biapplicative
import Data.Bifunctor.Join
import Data.Copointed
import Data.Foldable
import Data.Function
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import qualified Data.OrderedMap as Map
import Data.These
import Diff
import Info
import Patch
import Prelude hiding (fst, snd)
import Range
import Source hiding (break, fromList, uncons, (++))
import SplitDiff
import Syntax
import Term

-- | Assign line numbers to the lines on each side of a list of rows.
numberedRows :: [Join These a] -> [Join These (Int, a)]
numberedRows = countUp (both 1 1)
  where countUp from (row : rows) = fromJust ((,) <$> modifyJoin (uncurry These) from `applyThese` row) : countUp (modifyJoin (fromThese id id) (succ <$ row) <*> from) rows
        countUp _ [] = []

-- | Determine whether a line contains any patches.
hasChanges :: SplitDiff leaf Info -> Bool
hasChanges = or . (True <$)

type AlignedDiff leaf = [Join These (SplitDiff leaf Info)]

alignDiff :: Both (Source Char) -> Diff leaf Info -> AlignedDiff leaf
alignDiff sources diff = iter (uncurry (alignSyntax (runBothWith ((Join .) . These)) ((Free .) . Annotated) getRange sources) . (annotation &&& syntax)) (alignPatch sources <$> diff)

alignPatch :: Both (Source Char) -> Patch (Term leaf Info) -> AlignedDiff leaf
alignPatch sources patch = case patch of
  Delete term -> fmap (Pure . SplitDelete) <$> hylo (alignSyntax this (:<) getRange (Identity (fst sources))) unCofree (Identity <$> term)
  Insert term -> fmap (Pure . SplitInsert) <$> hylo (alignSyntax that (:<) getRange (Identity (snd sources))) unCofree (Identity <$> term)
  Replace term1 term2 -> fmap (Pure . SplitReplace) <$> alignWith (fmap (these id id const . runJoin) . Join)
    (hylo (alignSyntax this (:<) getRange (Identity (fst sources))) unCofree (Identity <$> term1))
    (hylo (alignSyntax that (:<) getRange (Identity (snd sources))) unCofree (Identity <$> term2))
  where getRange = characterRange . copoint
        this = Join . This . runIdentity
        that = Join . That . runIdentity

-- | The Applicative instance f is either Identity or Both. Identity is for Terms in Patches, Both is for Diffs in unchanged portions of the diff.
alignSyntax :: Applicative f => (forall a. f a -> Join These a) -> (Info -> Syntax leaf term -> term) -> (term -> Range) -> f (Source Char) -> f Info -> Syntax leaf [Join These term] -> [Join These term]
alignSyntax toJoinThese toNode getRange sources infos syntax = case syntax of
  Leaf s -> catMaybes $ wrapInBranch (const (Leaf s)) . fmap (flip (,) []) <$> sequenceL lineRanges
  Indexed children -> catMaybes $ wrapInBranch Indexed <$> alignBranch getRange children (modifyJoin (fromThese [] []) lineRanges)
  Fixed children -> catMaybes $ wrapInBranch Fixed <$> alignBranch getRange children (modifyJoin (fromThese [] []) lineRanges)
  Keyed children -> catMaybes $ wrapInBranch (Keyed . Map.fromList) <$> alignChildrenInRanges getRange lineRanges (Map.toList children)
  where lineRanges = toJoinThese $ actualLineRanges <$> (characterRange <$> infos) <*> sources
        wrapInBranch constructor = applyThese $ toJoinThese ((\ info (range, children) -> toNode (info { characterRange = range }) (constructor children)) <$> infos)

alignChildrenInRanges :: (Copointed c, Functor c, Foldable f) => (term -> Range) -> Join These [Range] -> f (c [Join These term]) -> [Join These (Range, [c term])]
alignChildrenInRanges getRange ranges children
  | Just headRanges <- sequenceL $ listToMaybe <$> ranges
  , (thisLine, nextLines, nonintersecting) <- spanAndSplitFirstLines (intersects getRange headRanges) children
  , thisRanges <- fromMaybe headRanges $ const <$> headRanges `applyThese` unionThese (distribute <$> (thisLine ++ (nextLines >>= distribute)))
  , merged <- pairRangesWithLine thisRanges (modifyJoin (uncurry These . fromThese [] []) (unionThese (distribute <$> thisLine)))
  , advance <- fromThese id id . runJoin . (drop 1 <$) $ unionThese (nextLines >>= copoint)
  , moreLines <- alignChildrenInRanges getRange (modifyJoin (uncurry bimap advance) ranges) (nextLines ++ nonintersecting)
  = merged : moreLines
  | otherwise = fmap (flip (,) []) <$> sequenceL ranges

{-


We align asymmetrically since the first child is asymmetrical, and then continue aligning symmetrically afterwards:
[         | [
  a       |
, b       |   b
]         | ]



[ [ Join This  (Range 4 5, [ pure (Delete (Info (Range 4 5) mempty 0 :< Leaf "a")) ]) ]
, [ Join These (Range 4 5, [ liftF (Info (Range 4 5) mempty 0 :< Leaf "b") ])
               (Range 4 5, [ liftF (Info (Range 4 5) mempty 0 :< Leaf "b") ])
]


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

[ [ Join That (Range 4 5, [ liftF (Info (Range 4 5) mempty 0 :< Leaf "b") ]) ] ]



We should avoid taking asymmetrical children greedily so as not to misalign asymmetrical children before symmetrical children on the same line:
         | [ a
[ b, c ] | , c
         | ]

-}

-- | Given a function to get the range, a list of already-aligned children, and the lists of ranges spanned by a branch, return the aligned lines.
alignBranch :: (term -> Range) -> [[Join These term]] -> Both [Range] -> [Join These (Range, [term])]
-- There are no more ranges, so we’re done.
alignBranch _ _ (Join ([], [])) = []
-- There are no more children, so we can just zip the remaining ranges together.
alignBranch _ [] ranges = runBothWith (alignWith Join) (fmap (flip (,) []) <$> ranges)
-- The first child is empty, and so can safely be dropped.
alignBranch getRange ([]:children) ranges = alignBranch getRange children ranges
-- There are both children and ranges, so we need to proceed line by line
alignBranch getRange children ranges = case intersectingChildren of
  -- No child intersects the current ranges on either side, so advance.
  [] -> (flip (,) [] <$> headRanges) : alignBranch getRange children (drop 1 <$> ranges)
  -- At least one child intersects on at least one side.
  _ -> if not $ any (and . intersectsFirstLine headRanges) children
    -- No child intersects on both sides, so align asymmetrically, picking the left side first to match the deletion/insertion order convention in diffs.
    then let (asymmetricalChildren, remainingIntersectingChildren) = break (isThese . runJoin . head) intersectingChildren
             (leftRange, rightRange) = splitThese headRanges in
      case fromThese False False . runJoin . intersectsFirstLine headRanges <$> listToMaybe remainingIntersectingChildren of
        Just (False, True) -> let (leftLine, remainingAtLeft) = maybe (id, []) (first (:)) $ lineAndRemaining asymmetricalChildren <$> leftRange in
          leftLine $ alignBranch getRange (remainingAtLeft ++ remainingIntersectingChildren ++ nonIntersectingChildren) (modifyJoin (uncurry bimap (advancePast [ [ Join (This ()) ] ])) ranges)
        Just (True, False) -> let (rightLine, remainingAtRight) = maybe (id, []) (first (:)) $ lineAndRemaining asymmetricalChildren <$> rightRange in
          rightLine $ alignBranch getRange (remainingAtRight ++ remainingIntersectingChildren ++ nonIntersectingChildren) (modifyJoin (uncurry bimap (advancePast [ [ Join (That ()) ] ])) ranges)
        Nothing -> let (leftLine, remainingAtLeft) = maybe (id, []) (first (:)) $ leftRange >>= lineAndRemainingWhere (any (isThis . runJoin . head)) asymmetricalChildren
                       (rightLine, remainingAtRight) = maybe (id, []) (first (:)) $ rightRange >>= lineAndRemainingWhere (any (isThat . runJoin . head)) asymmetricalChildren in
          leftLine $ rightLine $ alignBranch getRange (remainingAtLeft ++ remainingAtRight ++ nonIntersectingChildren) (modifyJoin (uncurry bimap (advancePast asymmetricalChildren)) ranges)
    -- | At least one child intersects on both sides, so align symmetrically.
    else let (line, remaining) = lineAndRemaining intersectingChildren headRanges in
      line : alignBranch getRange (remaining ++ nonIntersectingChildren) (drop 1 <$> ranges)
  where (intersectingChildren, nonIntersectingChildren) = span (or . intersectsFirstLine headRanges) children
        intersectsFirstLine ranges = maybe (False <$ ranges) (intersects getRange ranges) . listToMaybe
        Just headRanges = sequenceL $ listToMaybe <$> Join (runBothWith These ranges)
        lineAndRemaining children ranges = let (intersections, remaining) = alignChildren getRange ranges children in
          (fromJust ((,) <$> ranges `applyThese` Join (runBothWith These intersections)), remaining)
        lineAndRemainingWhere predicate children = if predicate children then Just . lineAndRemaining children else const Nothing
        advancePast children = fromThese id id . runJoin . (drop 1 <$) $ unionThese (head <$> children)

-- | Given a list of aligned children, produce lists of their intersecting first lines, and a list of the remaining lines/nonintersecting first lines.
alignChildren :: (term -> Range) -> Join These Range -> [[Join These term]] -> (Both [term], [[Join These term]])
alignChildren _ _ [] = (both [] [], [])
alignChildren getRange headRanges ([]:rest) = alignChildren getRange headRanges rest
alignChildren getRange headRanges ((firstLine:restOfLines):rest) = case fromThese False False . runJoin $ intersects getRange headRanges firstLine of
  -- It intersects on both sides, so we can just take the first line whole.
  (True, True) -> let (firstRemaining, restRemaining) = alignChildren getRange headRanges rest in
    ((++) <$> toTerms firstLine <*> firstRemaining, restOfLines : restRemaining)
  -- It only intersects on the left, so split it up.
  (True, False) -> let (firstRemaining, restRemaining) = alignChildren getRange headRanges rest in
    ((++) <$> toTerms (fromJust l) <*> firstRemaining, maybeToList r : restOfLines : restRemaining)
  -- It only intersects on the right, so split it up.
  (False, True) -> let (firstRemaining, restRemaining) = alignChildren getRange headRanges rest in
    ((++) <$> toTerms (fromJust r) <*> firstRemaining, maybeToList l : restOfLines : restRemaining)
  _ -> (both [] [], [])
  where toTerms line = modifyJoin (fromThese [] []) (pure <$> line)
        (l, r) = splitThese firstLine

{-

Properties of well-formed alignments:

- nodes are aligned in order, with priority given to earlier siblings over later ones (i.e. later nodes can be split up if necessary to allow earlier ones to be aligned)
- insertions alone on a line should not break the alignment of later siblings
- ✔︎ nodes should be aligned starting at the same line
- ✔︎ nodes should have the same height (# of lines spanned) on both sides, padding with blank lines as necessary

-}

spanAndSplitFirstLines :: (Copointed c, Functor c, Foldable f) => (Join These a -> Join These Bool) -> f (c [Join These a]) -> ([c (Join These a)], [c [Join These a]], [c [Join These a]])
spanAndSplitFirstLines pred = foldr (go pred) ([], [], [])
  where go pred child (this, next, nonintersecting)
          | (first : rest) <- copoint child
          , ~(l, r) <- splitThese first
          = case fromThese False False . runJoin $ pred first of
              (True, True) -> ((first <$ child) : this, (rest <$ child) : next, nonintersecting)
              (True, False) -> ((fromJust l <$ child) : this, (maybe rest (: rest) r <$ child) : next, nonintersecting)
              (False, True) -> ((fromJust r <$ child) : this, (maybe rest (: rest) l <$ child) : next, nonintersecting)
              _ -> (this, next, child : nonintersecting)
          | otherwise = (this, next, nonintersecting)

unionThese :: (Alternative f, Foldable f, Monoid (f a)) => f (Join These a) -> Join These (f a)
unionThese as = fromMaybe (Join (These empty empty)) . getUnion . fold $ Union . Just . fmap pure <$> as

pairRangesWithLine :: Monoid b => Join These a -> Join These b -> Join These (a, b)
pairRangesWithLine headRanges childLine = fromMaybe (flip (,) mempty <$> headRanges) $ (,) <$> headRanges `applyThese` childLine

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

-- Map over the bifunctor inside a Join, producing another Join.
modifyJoin :: (p a a -> q b b) -> Join p a -> Join q b
modifyJoin f = Join . f . runJoin

-- | Given a pair of Maybes, produce a These containing Just their values, or Nothing if they haven’t any.
maybeThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeThese (Just a) (Just b) = Just (These a b)
maybeThese (Just a) _ = Just (This a)
maybeThese _ (Just b) = Just (That b)
maybeThese _ _ = Nothing

-- Distributes a copointed functor through another functor.
--
-- This allows us to preserve any associated state while embedding the contents of the other functor into it.
distribute :: (Copointed c, Functor c, Functor f) => c (f a) -> f (c a)
distribute c = (<$ c) <$> copoint c

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
