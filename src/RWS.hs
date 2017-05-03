{-# LANGUAGE GADTs, DataKinds, RankNTypes, TypeOperators #-}
module RWS (
    rws
  , FeatureVector
  , stripDiff
  , defaultFeatureVectorDecorator
  , stripTerm
  , featureVectorDecorator
  , pqGramDecorator
  , Gram(..)
  , defaultD
  ) where

import Prologue
import Control.Monad.Effect as Eff
import Control.Monad.Effect.Internal as I
import Data.Record
import Data.These
import Patch
import Term
import Data.Array
import SES
import qualified Data.Functor.Both as Both

import Data.Functor.Listable
import Data.KdTree.Static hiding (toList)
import qualified Data.IntMap as IntMap
import Data.Semigroup (Min(..), Option(..))

import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Diff (mapAnnotations)

type Label f fields label = forall b. TermF f (Record fields) b -> label

type FeatureVector = Array Int Double

-- | A term which has not yet been mapped by `rws`, along with its feature vector summary & index.
data UnmappedTerm f fields = UnmappedTerm {
    termIndex :: Int -- ^ The index of the term within its root term.
  , feature   :: FeatureVector -- ^ Feature vector
  , term      :: Term f (Record fields) -- ^ The unmapped term
}

-- | Either a `term`, an index of a matched term, or nil.
data TermOrIndexOrNone term = Term term | Index Int | None

rws :: (HasField fields (Maybe FeatureVector), Foldable t, Functor f)
    => (Diff f fields -> Int)
    -> (Term f (Record fields) -> Term f (Record fields) -> Bool)
    -> t (Term f (Record fields))
    -> t (Term f (Record fields))
    -> RWSEditScript f fields
rws editDistance canCompare as bs = Eff.run . RWS.run editDistance canCompare as bs $ do
  sesDiffs <- ses'
  (featureAs, featureBs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs' sesDiffs
  (diffs, remaining) <- findNearestNeighoursToDiff' allDiffs featureAs featureBs
  diffs' <- deleteRemaining' diffs remaining
  rwsDiffs <- insertMapped' mappedDiffs diffs'
  pure (fmap snd rwsDiffs)

data RWS f fields result where
  SES :: RWS f fields (RWSEditScript f fields)

  GenFeaturizedTermsAndDiffs :: HasField fields (Maybe FeatureVector)
                             => RWSEditScript f fields
                             -> RWS f fields
                                ([UnmappedTerm f fields], [UnmappedTerm f fields], [MappedDiff f fields], [TermOrIndexOrNone (UnmappedTerm f fields)])

  FindNearestNeighoursToDiff :: [TermOrIndexOrNone (UnmappedTerm f fields)]
                             -> [UnmappedTerm f fields]
                             -> [UnmappedTerm f fields]
                             -> RWS f fields ([MappedDiff f fields], UnmappedTerms f fields)

  DeleteRemaining :: [MappedDiff f fields]
                  -> UnmappedTerms f fields
                  -> RWS f fields [MappedDiff f fields]

  InsertMapped :: [MappedDiff f fields] -> [MappedDiff f fields] -> RWS f fields [MappedDiff f fields]

-- | An IntMap of unmapped terms keyed by their position in a list of terms.
type UnmappedTerms f fields = IntMap (UnmappedTerm f fields)

type Diff f fields = These (Term f (Record fields)) (Term f (Record fields))

-- A Diff paired with both its indices
type MappedDiff f fields = (These Int Int, Diff f fields)

type RWSEditScript f fields = [Diff f fields]

run :: (Functor f, HasField fields (Maybe FeatureVector), Foldable t)
    => (Diff f fields -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
    -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
    -> t (Term f (Record fields))
    -> t (Term f (Record fields))
    -> Eff (RWS f fields ': e) (RWSEditScript f fields)
    -> Eff e (RWSEditScript f fields)
run editDistance canCompare as bs = relay pure (\m q -> q $ case m of
  SES -> ses canCompare as bs
  (GenFeaturizedTermsAndDiffs sesDiffs) ->
    evalState (genFeaturizedTermsAndDiffs sesDiffs) (0, 0)
  (FindNearestNeighoursToDiff allDiffs featureAs featureBs) ->
    findNearestNeighboursToDiff editDistance canCompare allDiffs featureAs featureBs
  (DeleteRemaining allDiffs remainingDiffs) ->
    deleteRemaining allDiffs remainingDiffs
  (InsertMapped allDiffs mappedDiffs) ->
    insertMapped allDiffs mappedDiffs)

insertMapped :: Foldable t => t (MappedDiff f fields) -> [MappedDiff f fields] -> [MappedDiff f fields]
insertMapped diffs into = foldl' (flip insertDiff) into diffs

deleteRemaining :: (Traversable t)
                => [MappedDiff f fields]
                -> t (UnmappedTerm f fields)
                -> [MappedDiff f fields]
deleteRemaining diffs unmappedAs =
  foldl' (flip insertDiff) diffs ((This . termIndex &&& This . term) <$> unmappedAs)

-- | Inserts an index and diff pair into a list of indices and diffs.
insertDiff :: MappedDiff f fields
           -> [MappedDiff f fields]
           -> [MappedDiff f fields]
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

findNearestNeighboursToDiff :: (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                           -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
                           -> [TermOrIndexOrNone (UnmappedTerm f fields)]
                           -> [UnmappedTerm f fields]
                           -> [UnmappedTerm f fields]
                           -> ([(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], UnmappedTerms f fields)
findNearestNeighboursToDiff editDistance canCompare allDiffs featureAs featureBs = (diffs, remaining)
  where
    (diffs, (_, remaining, _)) =
      traverse (findNearestNeighbourToDiff' editDistance canCompare (toKdTree <$> Both.both featureAs featureBs)) allDiffs &
      fmap catMaybes &
      (`runState` (minimumTermIndex featureAs, toMap featureAs, toMap featureBs))

findNearestNeighbourToDiff' :: (Diff f fields -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                           -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
                           -> Both.Both (KdTree Double (UnmappedTerm f fields))
                           -> TermOrIndexOrNone (UnmappedTerm f fields)
                           -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                                    (Maybe (MappedDiff f fields))
findNearestNeighbourToDiff' editDistance canCompare kdTrees termThing = case termThing of
  None -> pure Nothing
  Term term -> Just <$> findNearestNeighbourTo editDistance canCompare kdTrees term
  Index i -> do
    (_, unA, unB) <- get
    put (i, unA, unB)
    pure Nothing

-- | Construct a diff for a term in B by matching it against the most similar eligible term in A (if any), marking both as ineligible for future matches.
findNearestNeighbourTo :: (Diff f fields -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                       -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
                       -> Both.Both (KdTree Double (UnmappedTerm f fields))
                       -> UnmappedTerm f fields
                       -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                                (MappedDiff f fields)
findNearestNeighbourTo editDistance canCompare kdTrees term@(UnmappedTerm j _ b) = do
  (previous, unmappedA, unmappedB) <- get
  fromMaybe (insertion previous unmappedA unmappedB term) $ do
    -- Look up the nearest unmapped term in `unmappedA`.
    foundA@(UnmappedTerm i _ a) <- nearestUnmapped editDistance canCompare (IntMap.filterWithKey (\ k _ ->
      isInMoveBounds previous k)
      unmappedA) (Both.fst kdTrees) term
    -- Look up the nearest `foundA` in `unmappedB`
    UnmappedTerm j' _ _ <- nearestUnmapped editDistance canCompare unmappedB (Both.snd kdTrees) foundA
    -- Return Nothing if their indices don't match
    guard (j == j')
    guard (canCompare a b)
    pure $! do
      put (i, IntMap.delete i unmappedA, IntMap.delete j unmappedB)
      pure (These i j, These a b)

isInMoveBounds :: Int -> Int -> Bool
isInMoveBounds previous i = previous < i && i < previous + defaultMoveBound

-- | Finds the most-similar unmapped term to the passed-in term, if any.
--
-- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which have already been mapped, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
--
-- cf §4.2 of RWS-Diff
nearestUnmapped
  :: (Diff f fields -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
  -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
  -> UnmappedTerms f fields -- ^ A set of terms eligible for matching against.
  -> KdTree Double (UnmappedTerm f fields) -- ^ The k-d tree to look up nearest neighbours within.
  -> UnmappedTerm f fields -- ^ The term to find the nearest neighbour to.
  -> Maybe (UnmappedTerm f fields) -- ^ The most similar unmapped term, if any.
nearestUnmapped editDistance canCompare unmapped tree key = getFirst $ foldMap (First . Just) (sortOn (editDistanceIfComparable editDistance canCompare (term key) . term) (toList (IntMap.intersection unmapped (toMap (kNearest tree defaultL key)))))

editDistanceIfComparable :: Bounded t => (These a b -> t) -> (a -> b -> Bool) -> a -> b -> t
editDistanceIfComparable editDistance canCompare a b = if canCompare a b
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
             -> UnmappedTerms f fields
             -> UnmappedTerms f fields
             -> UnmappedTerm f fields
             -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                      (MappedDiff f fields)
insertion previous unmappedA unmappedB (UnmappedTerm j _ b) = do
  put (previous, unmappedA, IntMap.delete j unmappedB)
  pure (That j, That b)

genFeaturizedTermsAndDiffs :: (Functor f, HasField fields (Maybe FeatureVector))
                           => RWSEditScript f fields
                           -> State
                                (Int, Int)
                                ([UnmappedTerm f fields], [UnmappedTerm f fields], [MappedDiff f fields], [TermOrIndexOrNone (UnmappedTerm f fields)])
genFeaturizedTermsAndDiffs sesDiffs = case sesDiffs of
  [] -> pure ([], [], [], [])
  (diff : diffs) -> do
    (counterA, counterB) <- get
    case diff of
      This term -> do
        put (succ counterA, counterB)
        (as, bs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs diffs
        pure (featurize counterA term : as, bs, mappedDiffs, None : allDiffs )
      That term -> do
        put (counterA, succ counterB)
        (as, bs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs diffs
        pure (as, featurize counterB term : bs, mappedDiffs, Term (featurize counterB term) : allDiffs)
      These a b -> do
        put (succ counterA, succ counterB)
        (as, bs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs diffs
        pure (as, bs, (These counterA counterB, These a b) : mappedDiffs, Index counterA : allDiffs)

featurize :: (HasField fields (Maybe FeatureVector), Functor f) => Int -> Term f (Record fields) -> UnmappedTerm f fields
featurize index term = UnmappedTerm index (let Just v = getField (extract term) in v) (eraseFeatureVector term)

eraseFeatureVector :: (Functor f, HasField fields (Maybe FeatureVector)) => Term f (Record fields) -> Term f (Record fields)
eraseFeatureVector term = let record :< functor = runCofree term in
  cofree (setFeatureVector record Nothing :< functor)

setFeatureVector :: HasField fields (Maybe FeatureVector) => Record fields -> Maybe FeatureVector -> Record fields
setFeatureVector = setField

minimumTermIndex :: [RWS.UnmappedTerm f fields] -> Int
minimumTermIndex = pred . maybe 0 getMin . getOption . foldMap (Option . Just . Min . termIndex)

toMap :: [UnmappedTerm f fields] -> IntMap (UnmappedTerm f fields)
toMap = IntMap.fromList . fmap (termIndex &&& identity)

toKdTree :: [UnmappedTerm f fields] -> KdTree Double (UnmappedTerm f fields)
toKdTree = build (elems . feature)

-- Effect constructors

ses' :: (HasField fields (Maybe FeatureVector), RWS f fields :< e) => Eff e (RWSEditScript f fields)
ses' = send SES

genFeaturizedTermsAndDiffs' :: (HasField fields (Maybe FeatureVector), RWS f fields :< e)
                            => RWSEditScript f fields
                            -> Eff e ([UnmappedTerm f fields], [UnmappedTerm f fields], [MappedDiff f fields], [TermOrIndexOrNone (UnmappedTerm f fields)])
genFeaturizedTermsAndDiffs' = send . GenFeaturizedTermsAndDiffs

findNearestNeighoursToDiff' :: (RWS f fields :< e)
                            => [TermOrIndexOrNone (UnmappedTerm f fields)]
                            -> [UnmappedTerm f fields]
                            -> [UnmappedTerm f fields]
                            -> Eff e ([MappedDiff f fields], UnmappedTerms f fields)
findNearestNeighoursToDiff' diffs as bs = send (FindNearestNeighoursToDiff diffs as bs)

deleteRemaining' :: (RWS f fields :< e)
                 => [MappedDiff f fields]
                 -> UnmappedTerms f fields
                 -> Eff e [MappedDiff f fields]
deleteRemaining' diffs remaining = send (DeleteRemaining diffs remaining)

insertMapped' :: (RWS f fields :< e)
              => [MappedDiff f fields]
              -> [MappedDiff f fields]
              -> Eff e [MappedDiff f fields]
insertMapped' diffs mappedDiffs = send (InsertMapped diffs mappedDiffs)

-- | A `Gram` is a fixed-size view of some portion of a tree, consisting of a `stem` of _p_ labels for parent nodes, and a `base` of _q_ labels of sibling nodes. Collectively, the bag of `Gram`s for each node of a tree (e.g. as computed by `pqGrams`) form a summary of the tree.
data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
 deriving (Eq, Show)

-- | Annotates a term with a feature vector at each node, using the default values for the p, q, and d parameters.
defaultFeatureVectorDecorator
 :: (Hashable label, Traversable f)
 => Label f fields label
 -> Term f (Record fields)
 -> Term f (Record (Maybe FeatureVector ': fields))
defaultFeatureVectorDecorator getLabel = featureVectorDecorator getLabel defaultP defaultQ defaultD

-- | Annotates a term with a feature vector at each node, parameterized by stem length, base width, and feature vector dimensions.
featureVectorDecorator :: (Hashable label, Traversable f) => Label f fields label -> Int -> Int -> Int -> Term f (Record fields) -> Term f (Record (Maybe FeatureVector ': fields))
featureVectorDecorator getLabel p q d
 = cata collect
 . pqGramDecorator getLabel p q
 where collect ((gram :. rest) :< functor) = cofree ((foldl' addSubtermVector (Just (unitVector d (hash gram))) functor :. rest) :< functor)
       addSubtermVector :: Functor f => Maybe FeatureVector -> Term f (Record (Maybe FeatureVector ': fields)) -> Maybe FeatureVector
       addSubtermVector v term = addVectors <$> v <*> rhead (extract term)

       addVectors :: Num a => Array Int a -> Array Int a -> Array Int a
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
      cofree ((gram label :. headF term) :< assignParentAndSiblingLabels (tailF term) label)
    gram label = Gram (padToSize p []) (padToSize q (pure (Just label)))
    assignParentAndSiblingLabels functor label = (`evalState` (replicate (q `div` 2) Nothing <> siblingLabels functor)) (for functor (assignLabels label))

    assignLabels :: Functor f
                 => label
                 -> Term f (Record (Gram label ': fields))
                 -> State [Maybe label] (Term f (Record (Gram label ': fields)))
    assignLabels label a = case runCofree a of
      (gram :. rest) :< functor -> do
        labels <- get
        put (drop 1 labels)
        pure $! cofree ((gram { stem = padToSize p (Just label : stem gram), base = padToSize q labels } :. rest) :< functor)
    siblingLabels :: Traversable f => f (Term f (Record (Gram label ': fields))) -> [Maybe label]
    siblingLabels = foldMap (base . rhead . extract)
    padToSize n list = take n (list <> repeat Prologue.empty)

-- | Computes a unit vector of the specified dimension from a hash.
unitVector :: Int -> Int -> FeatureVector
unitVector d hash = fmap (* invMagnitude) uniform
  where
    uniform = listArray (0, d - 1) (evalRand components (pureMT (fromIntegral hash)))
    invMagnitude = 1 / sqrtDouble (sum (fmap (** 2) uniform))
    components = sequenceA (replicate d (liftRand randomDouble))

-- | Strips the head annotation off a term annotated with non-empty records.
stripTerm :: Functor f => Term f (Record (h ': t)) -> Term f (Record t)
stripTerm = fmap rtail

-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff
  :: (Functor f, Functor g)
  => Free (TermF f (g (Record (h ': t)))) (Patch (Term f (Record (h ': t))))
  -> Free (TermF f (g (Record t)))        (Patch (Term f (Record t)))
stripDiff = mapAnnotations rtail


-- Instances

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash gram = hash (stem gram <> base gram)

instance Listable1 Gram where
  liftTiers tiers = liftCons2 (liftTiers (liftTiers tiers)) (liftTiers (liftTiers tiers)) Gram

instance Listable a => Listable (Gram a) where
  tiers = tiers1
