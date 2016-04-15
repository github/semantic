{-# LANGUAGE RankNTypes #-}
module Alignment
( hasChanges
, numberedRows
, AlignedDiff
, alignDiff
, groupChildrenByLine
) where

import Control.Arrow ((&&&), (***))
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import Data.Align
import Data.Biapplicative
import Data.Bifunctor.Join
import Data.Copointed
import Data.Foldable
import Data.Functor.Both as Both
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.These
import Diff
import Info
import Patch
import Prelude hiding (fst, snd)
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
hasChanges :: SplitDiff leaf Info -> Bool
hasChanges = or . (True <$)

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
  , thisRanges <- fromMaybe headRanges $ const <$> headRanges `applyThese` Alignment.catThese (thisLine ++ nextLines)
  , merged <- pairRangesWithLine thisRanges (modifyJoin (uncurry These . fromThese [] []) (Alignment.catThese thisLine))
  , advance <- fromThese id id . runJoin . (drop 1 <$) $ Alignment.catThese nextLines
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

-- | Given a pair of Maybes, produce a These containing Just their values, or Nothing if they haven’t any.
maybeThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeThese (Just a) (Just b) = Just (These a b)
maybeThese (Just a) _ = Just (This a)
maybeThese _ (Just b) = Just (That b)
maybeThese _ _ = Nothing

-- | Like `<*>`, but it returns its result in `Maybe` since the result is the intersection of the shapes of the inputs.
apThese :: These (a -> b) (c -> d) -> These a c -> Maybe (These b d)
apThese fg ab = uncurry maybeThese $ uncurry (***) (bimap (<*>) (<*>) (unpack fg)) (unpack ab)
  where unpack = fromThese Nothing Nothing . bimap Just Just

-- | A Monoid wrapping These, for which mappend is the smallest shape covering both arguments.
newtype Union a b = Union { getUnion :: Maybe (These a b) }
  deriving (Eq, Show)


-- | Instances

instance (Monoid a, Monoid b) => Monoid (Union a b) where
  mempty = Union Nothing
  Union (Just a) `mappend` Union (Just b) = Union $ uncurry maybeThese $ uncurry (***) (bimap mappend mappend (unpack a)) (unpack b)
    where unpack = fromThese Nothing Nothing . bimap Just Just
  Union (Just a) `mappend` _ = Union $ Just a
  Union _ `mappend` Union (Just b) = Union $ Just b
  _ `mappend` _ = Union Nothing

instance Bicrosswalk t => Crosswalk (Join t) where
  crosswalk f = fmap Join . bicrosswalk f f . runJoin
