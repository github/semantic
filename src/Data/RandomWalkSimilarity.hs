{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.RandomWalkSimilarity
( rws
, pqGramDecorator
, defaultFeatureVectorDecorator
, featureVectorDecorator
, editDistanceUpTo
, defaultD
, defaultP
, defaultQ
, stripDiff
, stripTerm
, Gram(..)
, Label
) where

import Control.Applicative
import Control.Monad.Random
import Control.Monad.State
import Data.Align.Generic
import Data.Array
import Data.Functor.Both hiding (fst, snd)
import Data.Functor.Foldable
import Data.Functor.Listable
import Data.Hashable
import qualified Data.IntMap as IntMap
import qualified Data.KdTree.Static as KdTree
import Data.Record
import Data.Semigroup (Min(..), Option(..))
import Data.These
import Diff
import Info
import Patch
import Prologue as P
import qualified SES
import Term (termSize, zipTerms, Term, TermF)
import Test.QuickCheck.Random (mkQCGen)

type Label f fields label = forall b. TermF f (Record fields) b -> label
type DiffTerms f fields = Term f (Record fields) -> Term f (Record fields) -> Maybe (Diff f (Record fields))

-- | Given a function comparing two terms recursively,
-- a function to compute a Hashable label from an unpacked term, and two lists of terms,
-- compute the diff of a pair of lists of terms using a random walk similarity metric,
-- which completes in log-linear time.
--
-- This implementation is based on the paper [_RWS-Diff—Flexible and Efficient Change Detection in Hierarchical Data_](https://github.com/github/semantic-diff/files/325837/RWS-Diff.Flexible.and.Efficient.Change.Detection.in.Hierarchical.Data.pdf).
rws :: forall f fields label.
       (GAlign f, Traversable f, Eq (f (Term f Category)), Hashable label, HasField fields Category)
    => DiffTerms f fields -- ^ A function which compares a pair of terms recursively, returning 'Just' their diffed value if appropriate, or 'Nothing' if they should not be compared.
    -> Label f fields label
    -> [Term f (Record fields)] -- ^ The list of old terms.
    -> [Term f (Record fields)] -- ^ The list of new terms.
    -> [Diff f (Record fields)] -- ^ The resulting list of similarity-matched diffs.
rws compare getLabel as bs
  | null as, null bs = []
  | null as = inserting <$> bs
  | null bs = deleting <$> as
  | otherwise =
    -- Construct a State who's final value is a list of (Int, Diff leaf (Record fields))
    -- and who's final state is (Int, IntMap UmappedTerm, IntMap UmappedTerm)
    traverse findNearestNeighbourToDiff allDiffs &
    fmap catMaybes &
    -- Run the state with an initial state
    (`runState` (minimumTermIndex featurizedAs, toMap featurizedAs, toMap featurizedBs)) &
    uncurry deleteRemaining &
    insertMapped countersAndDiffs &
    fmap snd

  where
    minimumTermIndex = pred . maybe 0 getMin . getOption . foldMap (Option . Just . Min . termIndex)
    sesDiffs = eitherCutoff 1 <$> SES.ses replaceIfEqual cost as bs

    (featurizedAs, featurizedBs, _, _, countersAndDiffs, allDiffs) =
      foldl' (\(as, bs, counterA, counterB, diffs, allDiffs) diff -> case runFree diff of
        Pure (Right (Delete term)) ->
          (as <> pure (featurize counterA term), bs, succ counterA, counterB, diffs, allDiffs <> pure None)
        Pure (Right (Insert term)) ->
          (as, bs <> pure (featurize counterB term), counterA, succ counterB, diffs, allDiffs <> pure (Term (featurize counterB term)))
        syntax -> let diff' = free syntax >>= either identity pure in
          (as, bs, succ counterA, succ counterB, diffs <> pure (These counterA counterB, diff'), allDiffs <> pure (Index counterA))
      ) ([], [], 0, 0, [], []) sesDiffs

    findNearestNeighbourToDiff :: TermOrIndexOrNone (UnmappedTerm f fields)
                               -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                                        (Maybe (These Int Int, Diff f (Record fields)))
    findNearestNeighbourToDiff termThing = case termThing of
      None -> pure Nothing
      Term term -> Just <$> findNearestNeighbourTo term
      Index i -> do
        (_, unA, unB) <- get
        put (i, unA, unB)
        pure Nothing

    -- | Construct a diff for a term in B by matching it against the most similar eligible term in A (if any), marking both as ineligible for future matches.
    findNearestNeighbourTo :: UnmappedTerm f fields
                           -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                                    (These Int Int, Diff f (Record fields))
    findNearestNeighbourTo term@(UnmappedTerm j _ b) = do
      (previous, unmappedA, unmappedB) <- get
      fromMaybe (insertion previous unmappedA unmappedB term) $ do
        -- Look up the nearest unmapped term in `unmappedA`.
        foundA@(UnmappedTerm i _ a) <- nearestUnmapped (IntMap.filterWithKey (\ k _ ->
          isInMoveBounds previous k)
          unmappedA) kdas term
        -- Look up the nearest `foundA` in `unmappedB`
        UnmappedTerm j' _ _ <- nearestUnmapped unmappedB kdbs foundA
        -- Return Nothing if their indices don't match
        guard (j == j')
        compared <- compare a b
        pure $! do
          put (i, IntMap.delete i unmappedA, IntMap.delete j unmappedB)
          pure (These i j, compared)

    -- Returns a state (insertion index, old unmapped terms, new unmapped terms), and value of (index, inserted diff),
    -- given a previous index, two sets of umapped terms, and an unmapped term to insert.
    insertion :: Int
                 -> UnmappedTerms f fields
                 -> UnmappedTerms f fields
                 -> UnmappedTerm f fields
                 -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                          (These Int Int, Diff f (Record fields))
    insertion previous unmappedA unmappedB (UnmappedTerm j _ b) = do
      put (previous, unmappedA, IntMap.delete j unmappedB)
      pure (That j, inserting b)

    -- | Finds the most-similar unmapped term to the passed-in term, if any.
    --
    -- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which have already been mapped, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
    --
    -- cf §4.2 of RWS-Diff
    nearestUnmapped
      :: UnmappedTerms f fields -- ^ A set of terms eligible for matching against.
      -> KdTree.KdTree Double (UnmappedTerm f fields) -- ^ The k-d tree to look up nearest neighbours within.
      -> UnmappedTerm f fields -- ^ The term to find the nearest neighbour to.
      -> Maybe (UnmappedTerm f fields) -- ^ The most similar unmapped term, if any.
    nearestUnmapped unmapped tree key = getFirst $ foldMap (First . Just) (sortOn (maybe maxBound (editDistanceUpTo defaultM) . compare (term key) . term) (toList (IntMap.intersection unmapped (toMap (KdTree.kNearest tree defaultL key)))))

    insertMapped diffs into = foldl' (\into (i, mappedTerm) ->
        insertDiff (i, mappedTerm) into)
        into
        diffs

    -- Given a list of diffs, and unmapped terms, deletes any terms that remain in unmappedA.
    deleteRemaining diffs (_, unmappedA, _) = foldl' (\into (i, deletion) ->
        insertDiff (This i, deletion) into)
      diffs
      ((termIndex &&& deleting . term) <$> unmappedA)

    -- Possibly replace terms in a diff.
    replaceIfEqual :: Term f (Record fields) -> Term f (Record fields) -> Maybe (Diff f (Record fields))
    replaceIfEqual a b
      | (category <$> a) == (category <$> b) = hylo wrap runCofree <$> zipTerms a b
      | otherwise = Nothing

    cost = iter (const 0) . (1 <$)

    eitherCutoff :: Integer
                 -> Diff f (Record fields)
                 -> Free (TermF f (Both (Record fields)))
                         (Either (Diff f (Record fields)) (Patch (Term f (Record fields))))
    eitherCutoff n diff | n <= 0 = pure (Left diff)
    eitherCutoff n diff = free . bimap Right (eitherCutoff (pred n)) $ runFree diff

    kdas = KdTree.build (elems . feature) featurizedAs
    kdbs = KdTree.build (elems . feature) featurizedBs

    featurize :: Int -> Term f (Record fields) -> UnmappedTerm f fields
    featurize index term = UnmappedTerm index (rhead . extract $ defaultFeatureVectorDecorator getLabel term) term

    toMap = IntMap.fromList . fmap (termIndex &&& identity)


    -- | Determines whether an index is in-bounds for a move given the most recently matched index.
    isInMoveBounds previous i = previous < i && i < previous + defaultMoveBound

-- | Inserts an index and diff pair into a list of indices and diffs.
insertDiff :: (These Int Int, diff) -> [(These Int Int, diff)] -> [(These Int Int, diff)]
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

-- | Return an edit distance as the sum of it's term sizes, given an cutoff and a syntax of terms 'f a'.
-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (Foldable f, Functor f) => Integer -> Diff f annotation -> Int
editDistanceUpTo m = diffSum (patchSum termSize) . cutoff m
  where diffSum patchCost = sum . fmap (maybe 0 patchCost)

defaultD, defaultL, defaultP, defaultQ, defaultMoveBound :: Int
defaultD = 15
-- | How many of the most similar terms to consider, to rule out false positives.
defaultL = 2
defaultP = 2
defaultQ = 3
defaultMoveBound = 2

-- | How many nodes to consider for our constant-time approximation to tree edit distance.
defaultM :: Integer
defaultM = 10

-- | A term which has not yet been mapped by `rws`, along with its feature vector summary & index.
data UnmappedTerm f fields = UnmappedTerm {
    termIndex :: Int -- ^ The index of the term within its root term.
  , feature   :: FeatureVector -- ^ Feature vector
  , term      :: Term f (Record fields) -- ^ The unmapped term
}

-- | Either a `term`, an index of a matched term, or nil.
data TermOrIndexOrNone term = Term term | Index Int | None

-- | An IntMap of unmapped terms keyed by their position in a list of terms.
type UnmappedTerms f fields = IntMap (UnmappedTerm f fields)

type FeatureVector = Array Int Double

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

type VState = State (IntMap.IntMap FeatureVector)

-- | Annotates a term with a feature vector at each node, parameterized by stem length, base width, and feature vector dimensions.
featureVectorDecorator :: forall f label fields . (Hashable label, Traversable f) => Label f fields label -> Int -> Int -> Int -> Term f (Record fields) -> Term f (Record (FeatureVector ': fields))
featureVectorDecorator getLabel p q d
  = (`evalState` IntMap.empty) . cata collect . pqGramDecorator getLabel p q
  where collect :: CofreeF f (Record (Gram label ': fields)) (VState (Term f (Record (FeatureVector ': fields)))) -> VState (Term f (Record (FeatureVector ': fields)))
        collect ((gram :. rest) :< functorState) = do
          featureVector <- foldl' addSubtermVector (unitVector' d (hash gram)) functorState
          functor <- sequenceA functorState
          pure $! cofree ((featureVector :. rest) :< functor)
        addSubtermVector :: VState FeatureVector -> VState (Term f (Record (FeatureVector ': fields))) -> VState FeatureVector
        addSubtermVector accumState termState = do
          accum <- accumState
          term <- termState
          pure $! addVectors accum (rhead (extract term))

        addVectors :: Num a => Array Int a -> Array Int a -> Array Int a
        addVectors as bs = listArray (0, d - 1) (fmap (\ i -> as ! i + bs ! i) [0..(d - 1)])

unitVector' :: Int -> Int -> State (IntMap.IntMap FeatureVector) FeatureVector
unitVector' d hash = do
  map <- get
  case IntMap.lookup hash map of
    Just v -> pure v
    _ -> do
      let v = unitVector d hash
      put (IntMap.insert hash v map)
      pure v

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

    assignLabels :: label
                 -> Term f (Record (Gram label ': fields))
                 -> State [Maybe label] (Term f (Record (Gram label ': fields)))
    assignLabels label a = case runCofree a of
      (gram :. rest) :< functor -> do
        labels <- get
        put (drop 1 labels)
        pure $! cofree ((gram { stem = padToSize p (Just label : stem gram), base = padToSize q labels } :. rest) :< functor)
    siblingLabels :: Traversable f => f (Term f (Record (Gram label ': fields))) -> [Maybe label]
    siblingLabels = foldMap (base . rhead . extract)
    padToSize n list = take n (list <> repeat empty)

-- | Computes a unit vector of the specified dimension from a hash.
unitVector :: Int -> Int -> FeatureVector
unitVector d hash = fmap (/ magnitude) uniform
  where
    uniform = evalRand (listArray (0, d - 1) . take d <$> getRandoms) (mkQCGen hash)
    magnitude = sqrtDouble (sum (fmap (** 2) uniform))

-- | Strips the head annotation off a term annotated with non-empty records.
stripTerm :: Functor f => Term f (Record (h ': t)) -> Term f (Record t)
stripTerm = fmap rtail

-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff
  :: (Functor f, Functor g)
  => Free (TermF f (g (Record (h ': t)))) (Patch (Term f (Record (h ': t))))
  -> Free (TermF f (g (Record t)))        (Patch (Term f (Record t)))
stripDiff = iter (\ (h :< f) -> wrap (fmap rtail h :< f)) . fmap (pure . fmap stripTerm)


-- Instances

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash gram = hash (stem gram <> base gram)

instance Listable1 Gram where
  liftTiers tiers = liftCons2 (liftTiers (liftTiers tiers)) (liftTiers (liftTiers tiers)) Gram

instance Listable a => Listable (Gram a) where
  tiers = tiers1
