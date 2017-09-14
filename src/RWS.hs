{-# LANGUAGE GADTs, DataKinds, RankNTypes, TypeOperators #-}
module RWS (
    rws
  , ComparabilityRelation
  , FeatureVector
  , defaultFeatureVectorDecorator
  , featureVectorDecorator
  , pqGramDecorator
  , Gram(..)
  , defaultD
  ) where

import Control.Applicative (empty)
import Control.Arrow ((&&&))
import Control.Monad.State.Strict
import Data.Foldable
import Data.Function ((&))
import Data.Functor.Foldable
import Data.Hashable
import Data.List (sortOn)
import Data.Maybe
import Data.Monoid (First(..))
import Data.Record
import Data.Semigroup hiding (First(..))
import Data.These
import Data.Traversable
import Term
import Data.Array.Unboxed
import Data.Functor.Classes
import SES
import qualified Data.Functor.Both as Both
import Data.KdTree.Static hiding (empty, toList)
import qualified Data.IntMap as IntMap

import Control.Monad.Random
import System.Random.Mersenne.Pure64

type Label f fields label = forall b. TermF f (Record fields) b -> label

-- | A relation on 'Term's, guaranteed constant-time in the size of the 'Term' by parametricity.
--
--   This is used both to determine whether two root terms can be compared in O(1), and, recursively, to determine whether two nodes are equal in O(n); thus, comparability is defined s.t. two terms are equal if they are recursively comparable subterm-wise.
type ComparabilityRelation syntax ann1 ann2 = forall a b. TermF syntax ann1 a -> TermF syntax ann2 b -> Bool

type FeatureVector = UArray Int Double

-- | A term which has not yet been mapped by `rws`, along with its feature vector summary & index.
data UnmappedTerm syntax ann = UnmappedTerm {
    termIndex :: {-# UNPACK #-} !Int -- ^ The index of the term within its root term.
  , feature   :: {-# UNPACK #-} !FeatureVector -- ^ Feature vector
  , term      :: Term syntax ann -- ^ The unmapped term
}

-- | Either a `term`, an index of a matched term, or nil.
data TermOrIndexOrNone term = Term term | Index {-# UNPACK #-} !Int | None

rws :: (HasField fields FeatureVector, Functor syntax, Eq1 syntax)
    => (Diff syntax (Record fields) (Record fields) -> Int)
    -> ComparabilityRelation syntax (Record fields) (Record fields)
    -> [Term syntax (Record fields)]
    -> [Term syntax (Record fields)]
    -> RWSEditScript syntax (Record fields) (Record fields)
rws _            _          as [] = This <$> as
rws _            _          [] bs = That <$> bs
rws _            canCompare [a] [b] = if canCompareTerms canCompare a b then [These a b] else [That b, This a]
rws editDistance canCompare as bs =
  let sesDiffs = ses (equalTerms canCompare) as bs
      (featureAs, featureBs, mappedDiffs, allDiffs) = genFeaturizedTermsAndDiffs sesDiffs
      (diffs, remaining) = findNearestNeighboursToDiff editDistance canCompare allDiffs featureAs featureBs
      diffs' = deleteRemaining diffs remaining
      rwsDiffs = insertMapped mappedDiffs diffs'
  in fmap snd rwsDiffs

-- | An IntMap of unmapped terms keyed by their position in a list of terms.
type UnmappedTerms syntax ann = IntMap.IntMap (UnmappedTerm syntax ann)

type Diff syntax ann1 ann2 = These (Term syntax ann1) (Term syntax ann2)

-- A Diff paired with both its indices
type MappedDiff syntax ann1 ann2 = (These Int Int, Diff syntax ann1 ann2)

type RWSEditScript syntax ann1 ann2 = [Diff syntax ann1 ann2]

insertMapped :: Foldable t => t (MappedDiff syntax (Record fields) (Record fields)) -> [MappedDiff syntax (Record fields) (Record fields)] -> [MappedDiff syntax (Record fields) (Record fields)]
insertMapped diffs into = foldl' (flip insertDiff) into diffs

deleteRemaining :: (Traversable t)
                => [MappedDiff syntax (Record fields) (Record fields)]
                -> t (UnmappedTerm syntax (Record fields))
                -> [MappedDiff syntax (Record fields) (Record fields)]
deleteRemaining diffs unmappedAs =
  foldl' (flip insertDiff) diffs ((This . termIndex &&& This . term) <$> unmappedAs)

-- | Inserts an index and diff pair into a list of indices and diffs.
insertDiff :: MappedDiff syntax (Record fields) (Record fields)
           -> [MappedDiff syntax (Record fields) (Record fields)]
           -> [MappedDiff syntax (Record fields) (Record fields)]
insertDiff inserted [] = [ inserted ]
insertDiff a@(ij1, _) (b@(ij2, _):rest) = case (ij1, ij2) of
  (These i1 i2, These j1 j2) -> if i1 <= j1 && i2 <= j2 then a : b : rest else b : insertDiff a rest
  (This i, This j) -> if i <= j then a : b : rest else b : insertDiff a rest
  (That i, That j) -> if i <= j then a : b : rest else b : insertDiff a rest
  (This i, These j _) -> if i <= j then a : b : rest else b : insertDiff a rest
  (That i, These _ j) -> if i <= j then a : b : rest else b : insertDiff a rest

  (This _, That _) -> b : insertDiff a rest
  (That _, This _) -> b : insertDiff a rest

  (These i1 i2, _) -> case break (isThese . fst) rest of
    (rest, tail) -> let (before, after) = foldr' (combine i1 i2) ([], []) (b : rest) in
       case after of
         [] -> before <> insertDiff a tail
         _ -> before <> (a : after) <> tail
  where
    combine i1 i2 each (before, after) = case fst each of
      This j1 -> if i1 <= j1 then (before, each : after) else (each : before, after)
      That j2 -> if i2 <= j2 then (before, each : after) else (each : before, after)
      These _ _ -> (before, after)

findNearestNeighboursToDiff :: (These (Term syntax (Record fields)) (Term syntax (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                           -> ComparabilityRelation syntax (Record fields) (Record fields) -- ^ A relation determining whether two terms can be compared.
                           -> [TermOrIndexOrNone (UnmappedTerm syntax (Record fields))]
                           -> [UnmappedTerm syntax (Record fields)]
                           -> [UnmappedTerm syntax (Record fields)]
                           -> ([(These Int Int, These (Term syntax (Record fields)) (Term syntax (Record fields)))], UnmappedTerms syntax (Record fields))
findNearestNeighboursToDiff editDistance canCompare allDiffs featureAs featureBs = (diffs, remaining)
  where
    (diffs, (_, remaining, _)) =
      traverse (findNearestNeighbourToDiff' editDistance canCompare (toKdTree <$> Both.both featureAs featureBs)) allDiffs &
      fmap catMaybes &
      (`runState` (minimumTermIndex featureAs, toMap featureAs, toMap featureBs))

findNearestNeighbourToDiff' :: (Diff syntax (Record fields) (Record fields) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                           -> ComparabilityRelation syntax (Record fields) (Record fields) -- ^ A relation determining whether two terms can be compared.
                           -> Both.Both (KdTree Double (UnmappedTerm syntax (Record fields)))
                           -> TermOrIndexOrNone (UnmappedTerm syntax (Record fields))
                           -> State (Int, UnmappedTerms syntax (Record fields), UnmappedTerms syntax (Record fields))
                                    (Maybe (MappedDiff syntax (Record fields) (Record fields)))
findNearestNeighbourToDiff' editDistance canCompare kdTrees termThing = case termThing of
  None -> pure Nothing
  RWS.Term term -> Just <$> findNearestNeighbourTo editDistance canCompare kdTrees term
  Index i -> modify' (\ (_, unA, unB) -> (i, unA, unB)) >> pure Nothing

-- | Construct a diff for a term in B by matching it against the most similar eligible term in A (if any), marking both as ineligible for future matches.
findNearestNeighbourTo :: (Diff syntax (Record fields) (Record fields) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                       -> ComparabilityRelation syntax (Record fields) (Record fields) -- ^ A relation determining whether two terms can be compared.
                       -> Both.Both (KdTree Double (UnmappedTerm syntax (Record fields)))
                       -> UnmappedTerm syntax (Record fields)
                       -> State (Int, UnmappedTerms syntax (Record fields), UnmappedTerms syntax (Record fields))
                                (MappedDiff syntax (Record fields) (Record fields))
findNearestNeighbourTo editDistance canCompare kdTrees term@(UnmappedTerm j _ b) = do
  (previous, unmappedA, unmappedB) <- get
  fromMaybe (insertion previous unmappedA unmappedB term) $ do
    -- Look up the nearest unmapped term in `unmappedA`.
    foundA@(UnmappedTerm i _ a) <- nearestUnmapped editDistance canCompare (termsWithinMoveBoundsFrom previous unmappedA) (Both.fst kdTrees) term
    -- Look up the nearest `foundA` in `unmappedB`
    UnmappedTerm j' _ _ <- nearestUnmapped editDistance canCompare (termsWithinMoveBoundsFrom (pred j) unmappedB) (Both.snd kdTrees) foundA
    -- Return Nothing if their indices don't match
    guard (j == j')
    guard (canCompareTerms canCompare a b)
    pure $! do
      put (i, IntMap.delete i unmappedA, IntMap.delete j unmappedB)
      pure (These i j, These a b)
    where termsWithinMoveBoundsFrom bound = IntMap.filterWithKey (\ k _ -> isInMoveBounds bound k)

isInMoveBounds :: Int -> Int -> Bool
isInMoveBounds previous i = previous < i && i < previous + defaultMoveBound

-- | Finds the most-similar unmapped term to the passed-in term, if any.
--
-- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which have already been mapped, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
--
-- cf §4.2 of RWS-Diff
nearestUnmapped
  :: (Diff syntax (Record fields) (Record fields) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
  -> ComparabilityRelation syntax (Record fields) (Record fields) -- ^ A relation determining whether two terms can be compared.
  -> UnmappedTerms syntax (Record fields) -- ^ A set of terms eligible for matching against.
  -> KdTree Double (UnmappedTerm syntax (Record fields)) -- ^ The k-d tree to look up nearest neighbours within.
  -> UnmappedTerm syntax (Record fields) -- ^ The term to find the nearest neighbour to.
  -> Maybe (UnmappedTerm syntax (Record fields)) -- ^ The most similar unmapped term, if any.
nearestUnmapped editDistance canCompare unmapped tree key = getFirst $ foldMap (First . Just) (sortOn (editDistanceIfComparable editDistance canCompare (term key) . term) (toList (IntMap.intersection unmapped (toMap (kNearest tree defaultL key)))))

editDistanceIfComparable :: Bounded t
                         => (These (Term syntax (Record fields)) (Term syntax (Record fields)) -> t)
                         -> ComparabilityRelation syntax (Record fields) (Record fields)
                         -> Term syntax (Record fields)
                         -> Term syntax (Record fields)
                         -> t
editDistanceIfComparable editDistance canCompare a b = if canCompareTerms canCompare a b
  then editDistance (These a b)
  else maxBound

defaultD, defaultL, defaultP, defaultQ, defaultMoveBound :: Int
defaultD = 15
defaultL = 2
defaultP = 2
defaultQ = 3
defaultMoveBound = 2


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
  pure (That j, That b)

genFeaturizedTermsAndDiffs :: (Functor syntax, HasField fields FeatureVector)
                           => RWSEditScript syntax (Record fields) (Record fields)
                           -> ([UnmappedTerm syntax (Record fields)], [UnmappedTerm syntax (Record fields)], [MappedDiff syntax (Record fields) (Record fields)], [TermOrIndexOrNone (UnmappedTerm syntax (Record fields))])
genFeaturizedTermsAndDiffs sesDiffs = let Mapping _ _ a b c d = foldl' combine (Mapping 0 0 [] [] [] []) sesDiffs in (reverse a, reverse b, reverse c, reverse d)
  where combine (Mapping counterA counterB as bs mappedDiffs allDiffs) diff = case diff of
          This term -> Mapping (succ counterA) counterB (featurize counterA term : as) bs mappedDiffs (None : allDiffs)
          That term -> Mapping counterA (succ counterB) as (featurize counterB term : bs) mappedDiffs (RWS.Term (featurize counterB term) : allDiffs)
          These a b -> Mapping (succ counterA) (succ counterB) as bs ((These counterA counterB, These a b) : mappedDiffs) (Index counterA : allDiffs)

data Mapping syntax fields = Mapping {-# UNPACK #-} !Int {-# UNPACK #-} !Int ![UnmappedTerm syntax (Record fields)] ![UnmappedTerm syntax (Record fields)] ![MappedDiff syntax (Record fields) (Record fields)] ![TermOrIndexOrNone (UnmappedTerm syntax (Record fields))]

featurize :: (HasField fields FeatureVector, Functor syntax) => Int -> Term syntax (Record fields) -> UnmappedTerm syntax (Record fields)
featurize index term = UnmappedTerm index (getField (extract term)) (eraseFeatureVector term)

eraseFeatureVector :: (Functor f, HasField fields FeatureVector) => Term f (Record fields) -> Term f (Record fields)
eraseFeatureVector (Term.Term (In record functor)) = termIn (setFeatureVector record nullFeatureVector) functor

nullFeatureVector :: FeatureVector
nullFeatureVector = listArray (0, 0) [0]

setFeatureVector :: HasField fields FeatureVector => Record fields -> FeatureVector -> Record fields
setFeatureVector = setField

minimumTermIndex :: [UnmappedTerm syntax ann] -> Int
minimumTermIndex = pred . maybe 0 getMin . getOption . foldMap (Option . Just . Min . termIndex)

toMap :: [UnmappedTerm syntax ann] -> IntMap.IntMap (UnmappedTerm syntax ann)
toMap = IntMap.fromList . fmap (termIndex &&& id)

toKdTree :: [UnmappedTerm syntax ann] -> KdTree Double (UnmappedTerm syntax ann)
toKdTree = build (elems . feature)

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

       addVectors :: UArray Int Double -> UArray Int Double -> UArray Int Double
       addVectors as bs = listArray (0, d - 1) (fmap (\ i -> as ! i + bs ! i) [0..(d - 1)])

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
unitVector d hash = listArray (0, d - 1) ((* invMagnitude) <$> components)
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


-- Instances

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash gram = hash (stem gram <> base gram)
