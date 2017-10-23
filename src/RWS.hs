{-# LANGUAGE GADTs, DataKinds, RankNTypes, TypeOperators #-}
module RWS
( rws
, ComparabilityRelation
, FeatureVector(..)
, defaultFeatureVectorDecorator
, featureVectorDecorator
, pqGramDecorator
, Gram(..)
, defaultD
, canCompareTerms
, equalTerms
) where

import Control.Applicative (empty)
import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Align.Generic
import Data.Array.Unboxed
import Data.Bifunctor (bimap)
import Data.Diff (DiffF(..), deleting, inserting, merge, replacing)
import Data.Foldable
import Data.Function ((&))
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Hashable
import qualified Data.IntMap as IntMap
import Data.KdMap.Static hiding (elems, empty, inRange)
import Data.List (sortOn)
import Data.Maybe
import Data.Record
import Data.Semigroup hiding (First(..))
import Data.Term as Term
import Data.These
import Data.Traversable
import SES
import System.Random.Mersenne.Pure64

type Label f fields label = forall b. TermF f (Record fields) b -> label

-- | A relation on 'Term's, guaranteed constant-time in the size of the 'Term' by parametricity.
--
--   This is used both to determine whether two root terms can be compared in O(1), and, recursively, to determine whether two nodes are equal in O(n); thus, comparability is defined s.t. two terms are equal if they are recursively comparable subterm-wise.
type ComparabilityRelation syntax ann1 ann2 = forall a b. TermF syntax ann1 a -> TermF syntax ann2 b -> Bool

newtype FeatureVector = FV { unFV :: UArray Int Double }
  deriving (Eq, Ord, Show)

-- | A term which has not yet been mapped by `rws`, along with its feature vector summary & index.
data UnmappedTerm syntax ann = UnmappedTerm
  { termIndex :: {-# UNPACK #-} !Int -- ^ The index of the term within its root term.
  , feature   :: {-# UNPACK #-} !FeatureVector -- ^ Feature vector
  , term      :: Term syntax ann -- ^ The unmapped term
  }

-- | Either a `term`, an index of a matched term, or nil.
data TermOrIndexOrNone term = Term term | Index {-# UNPACK #-} !Int | None

rws :: (Foldable syntax, Functor syntax, GAlign syntax)
    => ComparabilityRelation syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2))
    -> (Term syntax (Record (FeatureVector ': fields1)) -> Term syntax (Record (FeatureVector ': fields2)) -> Bool)
    -> [Term syntax (Record (FeatureVector ': fields1))]
    -> [Term syntax (Record (FeatureVector ': fields2))]
    -> RWSEditScript syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2))
rws _          _          as [] = This <$> as
rws _          _          [] bs = That <$> bs
rws canCompare _          [a] [b] = if canCompareTerms canCompare a b then [These a b] else [That b, This a]
rws canCompare equivalent as bs
  = ses equivalent as bs
  & genFeaturizedTermsAndDiffs
  & \ (featureAs, featureBs, mappedDiffs, allDiffs) ->
      findNearestNeighboursToDiff canCompare allDiffs featureAs featureBs
    & uncurry deleteRemaining
    & insertMapped mappedDiffs
    & fmap (bimap snd snd)

-- | An IntMap of unmapped terms keyed by their position in a list of terms.
type UnmappedTerms syntax ann = IntMap.IntMap (UnmappedTerm syntax ann)

type Edit syntax ann1 ann2 = These (Term syntax ann1) (Term syntax ann2)

-- A Diff paired with both its indices
type MappedDiff syntax ann1 ann2 = These (Int, Term syntax ann1) (Int, Term syntax ann2)

type RWSEditScript syntax ann1 ann2 = [Edit syntax ann1 ann2]

insertMapped :: Foldable t
             => t (MappedDiff syntax ann1 ann2)
             -> [MappedDiff syntax ann1 ann2]
             -> [MappedDiff syntax ann1 ann2]
insertMapped diffs into = foldl' (flip insertDiff) into diffs

deleteRemaining :: Traversable t
                => [MappedDiff syntax ann1 ann2]
                -> t (UnmappedTerm syntax ann1)
                -> [MappedDiff syntax ann1 ann2]
deleteRemaining diffs remaining = insertMapped (This . (termIndex &&& term) <$> remaining) diffs

-- | Inserts an index and diff pair into a list of indices and diffs.
insertDiff :: MappedDiff syntax ann1 ann2
           -> [MappedDiff syntax ann1 ann2]
           -> [MappedDiff syntax ann1 ann2]
insertDiff inserted [] = [ inserted ]
insertDiff a (b:rest) = case (bimap fst fst a, bimap fst fst b) of
  (These i1 i2, These j1 j2) -> if i1 <= j1 && i2 <= j2 then a : b : rest else b : insertDiff a rest
  (This i, This j) -> if i <= j then a : b : rest else b : insertDiff a rest
  (That i, That j) -> if i <= j then a : b : rest else b : insertDiff a rest
  (This i, These j _) -> if i <= j then a : b : rest else b : insertDiff a rest
  (That i, These _ j) -> if i <= j then a : b : rest else b : insertDiff a rest

  (This _, That _) -> b : insertDiff a rest
  (That _, This _) -> b : insertDiff a rest

  (These i1 i2, _) -> case break isThese rest of
    (rest, tail) -> let (before, after) = foldr' (combine i1 i2) ([], []) (b : rest) in
       case after of
         [] -> before <> insertDiff a tail
         _ -> before <> (a : after) <> tail
  where
    combine i1 i2 each (before, after) = case bimap fst fst each of
      This j1 -> if i1 <= j1 then (before, each : after) else (each : before, after)
      That j2 -> if i2 <= j2 then (before, each : after) else (each : before, after)
      These _ _ -> (before, after)

findNearestNeighboursToDiff :: (Foldable syntax, Functor syntax, GAlign syntax)
                            => ComparabilityRelation syntax ann1 ann2 -- ^ A relation determining whether two terms can be compared.
                            -> [TermOrIndexOrNone (UnmappedTerm syntax ann2)]
                            -> [UnmappedTerm syntax ann1]
                            -> [UnmappedTerm syntax ann2]
                            -> ([MappedDiff syntax ann1 ann2], UnmappedTerms syntax ann1)
findNearestNeighboursToDiff canCompare allDiffs featureAs featureBs = (diffs, remaining)
  where
    (diffs, (_, remaining, _)) =
      traverse (findNearestNeighbourToDiff' canCompare (toKdMap featureAs) (toKdMap featureBs)) allDiffs &
      fmap catMaybes &
      (`runState` (pred (maybe 0 termIndex (listToMaybe featureAs)), toMap featureAs, toMap featureBs))

findNearestNeighbourToDiff' :: (Foldable syntax, Functor syntax, GAlign syntax)
                            => ComparabilityRelation syntax ann1 ann2 -- ^ A relation determining whether two terms can be compared.
                            -> KdMap Double FeatureVector (UnmappedTerm syntax ann1)
                            -> KdMap Double FeatureVector (UnmappedTerm syntax ann2)
                            -> TermOrIndexOrNone (UnmappedTerm syntax ann2)
                            -> State (Int, UnmappedTerms syntax ann1, UnmappedTerms syntax ann2)
                                     (Maybe (MappedDiff syntax ann1 ann2))
findNearestNeighbourToDiff' canCompare kdTreeA kdTreeB termThing = case termThing of
  None -> pure Nothing
  RWS.Term term -> Just <$> findNearestNeighbourTo canCompare kdTreeA kdTreeB term
  Index i -> modify' (\ (_, unA, unB) -> (i, unA, unB)) >> pure Nothing

-- | Construct a diff for a term in B by matching it against the most similar eligible term in A (if any), marking both as ineligible for future matches.
findNearestNeighbourTo :: (Foldable syntax, Functor syntax, GAlign syntax)
                       => ComparabilityRelation syntax ann1 ann2 -- ^ A relation determining whether two terms can be compared.
                       -> KdMap Double FeatureVector (UnmappedTerm syntax ann1)
                       -> KdMap Double FeatureVector (UnmappedTerm syntax ann2)
                       -> UnmappedTerm syntax ann2
                       -> State (Int, UnmappedTerms syntax ann1, UnmappedTerms syntax ann2)
                                (MappedDiff syntax ann1 ann2)
findNearestNeighbourTo canCompare kdTreeA kdTreeB term@(UnmappedTerm j _ b) = do
  (previous, unmappedA, unmappedB) <- get
  fromMaybe (insertion previous unmappedA unmappedB term) $ do
    -- Look up the nearest unmapped term in `unmappedA`.
    foundA@(UnmappedTerm i _ a) <- nearestUnmapped (nearAndComparableTo canCompare previous b unmappedA) kdTreeA term
    -- Look up the nearest `foundA` in `unmappedB`
    UnmappedTerm j' _ _ <- nearestUnmapped ((nearAndComparableTo (flip canCompare) (pred j) a) unmappedB) kdTreeB foundA
    -- Return Nothing if their indices don't match
    guard (j == j')
    pure $! do
      put (i, IntMap.delete i unmappedA, IntMap.delete j unmappedB)
      pure (These (i, a) (j, b))

nearAndComparableTo :: ComparabilityRelation syntax ann1 ann2 -> Int -> Term syntax ann2 -> UnmappedTerms syntax ann1 -> UnmappedTerms syntax ann1
nearAndComparableTo canCompare index term = IntMap.filter (\ (UnmappedTerm k _ term') -> inRange (succ index, index + defaultMoveBound) k && canCompareTerms canCompare term' term)

-- | Finds the most-similar unmapped term to the passed-in term, if any.
--
-- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which have already been mapped, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
--
-- cf §4.2 of RWS-Diff
nearestUnmapped :: (Foldable syntax, Functor syntax, GAlign syntax)
                => UnmappedTerms syntax ann1 -- ^ A set of terms eligible for matching against.
                -> KdMap Double FeatureVector (UnmappedTerm syntax ann1) -- ^ The k-d tree to look up nearest neighbours within.
                -> UnmappedTerm syntax ann2 -- ^ The term to find the nearest neighbour to.
                -> Maybe (UnmappedTerm syntax ann1) -- ^ The most similar unmapped term, if any.
nearestUnmapped unmapped tree key = listToMaybe (sortOn approximateEditDistance candidates)
  where candidates = toList (IntMap.intersection unmapped (toMap (fmap snd (kNearest tree defaultL (feature key)))))
        approximateEditDistance = editDistanceUpTo defaultM (term key) . term

defaultD, defaultL, defaultP, defaultQ, defaultMoveBound :: Int
defaultD = 15
defaultL = 2
defaultP = 2
defaultQ = 3
defaultMoveBound = 1


-- Returns a state (insertion index, old unmapped terms, new unmapped terms), and value of (index, inserted diff),
-- given a previous index, two sets of umapped terms, and an unmapped term to insert.
insertion :: Int
          -> UnmappedTerms syntax ann1
          -> UnmappedTerms syntax ann2
          -> UnmappedTerm syntax ann2
          -> State (Int, UnmappedTerms syntax ann1, UnmappedTerms syntax ann2)
                   (MappedDiff syntax ann1 ann2)
insertion previous unmappedA unmappedB (UnmappedTerm j _ b) = do
  put (previous, unmappedA, IntMap.delete j unmappedB)
  pure (That (j, b))

genFeaturizedTermsAndDiffs :: Functor syntax
                           => RWSEditScript syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2))
                           -> ( [UnmappedTerm syntax (Record (FeatureVector ': fields1))]
                              , [UnmappedTerm syntax (Record (FeatureVector ': fields2))]
                              , [MappedDiff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2))]
                              , [TermOrIndexOrNone (UnmappedTerm syntax (Record (FeatureVector ': fields2)))]
                              )
genFeaturizedTermsAndDiffs sesDiffs = let Mapping _ _ a b c d = foldl' combine (Mapping 0 0 [] [] [] []) sesDiffs in (reverse a, reverse b, reverse c, reverse d)
  where combine (Mapping counterA counterB as bs mappedDiffs allDiffs) diff = case diff of
          This term -> Mapping (succ counterA) counterB (featurize counterA term : as) bs mappedDiffs (None : allDiffs)
          That term -> Mapping counterA (succ counterB) as (featurize counterB term : bs) mappedDiffs (RWS.Term (featurize counterB term) : allDiffs)
          These a b -> Mapping (succ counterA) (succ counterB) as bs ((These (counterA, a) (counterB, b)) : mappedDiffs) (Index counterA : allDiffs)

data Mapping syntax ann1 ann2
  = Mapping
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    ![UnmappedTerm syntax ann1]
    ![UnmappedTerm syntax ann2]
    ![MappedDiff syntax ann1 ann2]
    ![TermOrIndexOrNone (UnmappedTerm syntax ann2)]

featurize :: Functor syntax => Int -> Term syntax (Record (FeatureVector ': fields)) -> UnmappedTerm syntax (Record (FeatureVector ': fields))
featurize index term = UnmappedTerm index (getField (extract term)) (eraseFeatureVector term)

eraseFeatureVector :: Functor syntax => Term syntax (Record (FeatureVector ': fields)) -> Term syntax (Record (FeatureVector ': fields))
eraseFeatureVector (Term.Term (In record functor)) = termIn (setFeatureVector record nullFeatureVector) functor

nullFeatureVector :: FeatureVector
nullFeatureVector = FV $ listArray (0, 0) [0]

setFeatureVector :: Record (FeatureVector ': fields) -> FeatureVector -> Record (FeatureVector ': fields)
setFeatureVector = setField

toMap :: [UnmappedTerm syntax ann] -> IntMap.IntMap (UnmappedTerm syntax ann)
toMap = IntMap.fromList . fmap (termIndex &&& id)

toKdMap :: [UnmappedTerm syntax ann] -> KdMap Double FeatureVector (UnmappedTerm syntax ann)
toKdMap = build (elems . unFV) . fmap (feature &&& id)

-- | A `Gram` is a fixed-size view of some portion of a tree, consisting of a `stem` of _p_ labels for parent nodes, and a `base` of _q_ labels of sibling nodes. Collectively, the bag of `Gram`s for each node of a tree (e.g. as computed by `pqGrams`) form a summary of the tree.
data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
 deriving (Eq, Show)

-- | Annotates a term with a feature vector at each node, using the default values for the p, q, and d parameters.
defaultFeatureVectorDecorator
 :: (Hashable label, Traversable f)
 => Label f fields label
 -> Term f (Record fields)
 -> Term f (Record (FeatureVector ': fields))
defaultFeatureVectorDecorator getLabel = featureVectorDecorator getLabel defaultP defaultQ defaultD

-- | Annotates a term with a feature vector at each node, parameterized by stem length, base width, and feature vector dimensions.
featureVectorDecorator :: (Hashable label, Traversable f) => Label f fields label -> Int -> Int -> Int -> Term f (Record fields) -> Term f (Record (FeatureVector ': fields))
featureVectorDecorator getLabel p q d
 = cata collect
 . pqGramDecorator getLabel p q
 where collect (In (gram :. rest) functor) = termIn (foldl' addSubtermVector (unitVector d (hash gram)) functor :. rest) functor
       addSubtermVector :: Functor f => FeatureVector -> Term f (Record (FeatureVector ': fields)) -> FeatureVector
       addSubtermVector v term = addVectors v (rhead (extract term))

       addVectors :: FeatureVector -> FeatureVector -> FeatureVector
       addVectors (FV as) (FV bs) = FV $ listArray (0, d - 1) (fmap (\ i -> as ! i + bs ! i) [0..(d - 1)])

-- | Annotates a term with the corresponding p,q-gram at each node.
pqGramDecorator
  :: Traversable f
  => Label f fields label -- ^ A function computing the label from an arbitrary unpacked term. This function can use the annotation and functor’s constructor, but not any recursive values inside the functor (since they’re held parametric in 'b').
  -> Int -- ^ 'p'; the desired stem length for the grams.
  -> Int -- ^ 'q'; the desired base length for the grams.
  -> Term f (Record fields) -- ^ The term to decorate.
  -> Term f (Record (Gram label ': fields)) -- ^ The decorated term.
pqGramDecorator getLabel p q = cata algebra
  where
    algebra term = let label = getLabel term in
      termIn (gram label :. termAnnotation term) (assignParentAndSiblingLabels (termOut term) label)
    gram label = Gram (padToSize p []) (padToSize q (pure (Just label)))
    assignParentAndSiblingLabels functor label = (`evalState` (replicate (q `div` 2) Nothing <> siblingLabels functor)) (for functor (assignLabels label))

    assignLabels :: Functor f
                 => label
                 -> Term f (Record (Gram label ': fields))
                 -> State [Maybe label] (Term f (Record (Gram label ': fields)))
    assignLabels label (Term.Term (In (gram :. rest) functor)) = do
      labels <- get
      put (drop 1 labels)
      pure $! termIn (gram { stem = padToSize p (Just label : stem gram), base = padToSize q labels } :. rest) functor
    siblingLabels :: Traversable f => f (Term f (Record (Gram label ': fields))) -> [Maybe label]
    siblingLabels = foldMap (base . rhead . extract)
    padToSize n list = take n (list <> repeat empty)

-- | Computes a unit vector of the specified dimension from a hash.
unitVector :: Int -> Int -> FeatureVector
unitVector d hash = FV $ listArray (0, d - 1) ((* invMagnitude) <$> components)
  where
    invMagnitude = 1 / sqrt (sum (fmap (** 2) components))
    components = evalRand (sequenceA (replicate d (liftRand randomDouble))) (pureMT (fromIntegral hash))

-- | Test the comparability of two root 'Term's in O(1).
canCompareTerms :: ComparabilityRelation syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Bool
canCompareTerms canCompare t1 t2 = canCompare (unTerm t1) (unTerm t2)

-- | Recursively test the equality of two 'Term's in O(n).
equalTerms :: Eq1 syntax => ComparabilityRelation syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Bool
equalTerms canCompare = go
  where go a b = canCompareTerms canCompare a b && liftEq go (termOut (unTerm a)) (termOut (unTerm b))


-- | How many nodes to consider for our constant-time approximation to tree edit distance.
defaultM :: Integer
defaultM = 10

-- | Return an edit distance as the sum of it's term sizes, given an cutoff and a syntax of terms 'f a'.
-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (GAlign syntax, Foldable syntax, Functor syntax) => Integer -> Term syntax ann1 -> Term syntax ann2 -> Int
editDistanceUpTo m a b = diffCost m (approximateDiff a b)
  where diffCost = flip . cata $ \ diff m -> case diff of
          _ | m <= 0 -> 0
          Merge body -> sum (fmap ($ pred m) body)
          body -> succ (sum (fmap ($ pred m) body))
        approximateDiff a b = maybe (replacing a b) (merge (extract a, extract b)) (galignWith (Just . these deleting inserting approximateDiff) (unwrap a) (unwrap b))


-- Instances

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash gram = hash (stem gram <> base gram)
