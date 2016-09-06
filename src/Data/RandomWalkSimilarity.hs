{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.RandomWalkSimilarity
( rws
, pqGramDecorator
, defaultFeatureVectorDecorator
, hashDecorator
, featureVectorDecorator
, editDistanceUpTo
, defaultD
, defaultP
, defaultQ
, stripDiff
, stripTerm
, Gram(..)
) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad.State
import Data.Functor.Both hiding (fst, snd)
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.IntMap as IntMap
import qualified Data.KdTree.Static as KdTree
import qualified Data.List as List
import Data.Record
import qualified Data.Vector as Vector
import Patch
import Prologue
import Term (termSize, zipTerms)
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Random
import qualified Data.PQueue.Prio.Max as PQueue
import qualified SES as SES
import Info
import Data.Align.Generic

-- | Given a function comparing two terms recursively, and a function to compute a Hashable label from an unpacked term, compute the diff of a pair of lists of terms using a random walk similarity metric, which completes in log-linear time. This implementation is based on the paper [_RWS-Diff—Flexible and Efficient Change Detection in Hierarchical Data_](https://github.com/github/semantic-diff/files/325837/RWS-Diff.Flexible.and.Efficient.Change.Detection.in.Hierarchical.Data.pdf).
rws :: forall f fields. (Eq (Record fields), GAlign f, HasField fields Category, Traversable f, Eq (f (Cofree f Category)), HasField fields (Vector.Vector Double), HasField fields Int)
    => (Cofree f (Record fields)
       -> Cofree f (Record fields)
       -> Maybe (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields))))) -- ^ A function which compares a pair of terms recursively, returning 'Just' their diffed value if appropriate, or 'Nothing' if they should not be compared.
    -> [Cofree f (Record fields)] -- ^ The list of old terms.
    -> [Cofree f (Record fields)] -- ^ The list of new terms.
    -> [Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))] -- ^ The resulting list of similarity-matched diffs.
rws compare as bs
  | null as, null bs = []
  | null as = inserting <$> bs
  | null bs = deleting <$> as
  | otherwise =
    -- Construct a State who's final value is a list of (Int, Diff leaf (Record fields))
    -- and who's final state is (Int, IntMap UmappedTerm, IntMap UmappedTerm)
    traverse findNearestNeighbourTo fbs &
    -- Run the state with an initial state
    (`runState` (negate 1, toMap fas, toMap fbs)) &
    uncurry deleteRemaining &
    fmap snd
    -- Modified xydiff + RWS
    -- 1. Annotate each node with a unique key top down based off its categories and termIndex?
    -- 2. Construct two priority queues of hash values for each node ordered by max weight
    -- 3. Try to find matchings starting with the heaviest nodes
    -- 4. Use structure to propagate matchings?
    -- 5. Compute the diff

  where queueAs = PQueue.fromList (zipWith hashabilize [0..] as)
        queueBs = PQueue.fromList (zipWith hashabilize [0..] bs)
        sesDiff = SES.ses replaceIfEqual cost as bs
        replaceIfEqual a b
          | (category <$> a) == (category <$> b) = hylo wrap runCofree <$> zipTerms a b
          | otherwise = Nothing
        cost = iter (const 0) . (1 <$)

        -- sesDiff' :: [ Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields))) ] -> ([ Either Diff (Either UnmappedTerm UnmappedTerm) ])
        -- sesDiff' diffs =  foldl' (\(as, bs) diff -> ([], [])) mempty diffs
        --   where go (Replace a b :< s) = cofree . ((a : as, b : bs) :<) <$> sequenceA s





        -- (fas, fbs) = iter sesDiff (\case
        --   Replace a b -> (a, b)
        --   Insert b -> ()
        --   otherwise -> expression)
        (fas, fbs, _, _) = foldl' (\(as, bs, counterA, counterB) diff -> case runFree diff of
          Pure (Delete term) -> (featurize counterA term : as, bs, succ counterA, counterB)
          Pure (Insert term) -> (as, featurize counterB term : bs, counterA, succ counterB)
          _ -> (as, bs, succ counterA, succ counterB)
          ) ([], [], 0, 0) sesDiff

        -- (catMaybes $ (\ diff -> case runFree diff of { Pure (Delete term) -> UnmappedTerm  }) <$> sesDiff, )

        dropEqualTerms :: PQueue.MaxPQueue Int (UnmappedHashTerm (Cofree f (Record fields))) -> PQueue.MaxPQueue Int (UnmappedHashTerm (Cofree f (Record fields))) ->  ([UnmappedTerm (Cofree f (Record fields))], [UnmappedTerm (Cofree f (Record fields))])
        dropEqualTerms as bs = (unmappedAs, unmappedBs)
          where
            (unmappedAs, unmappedBs, _, _) = dropEqualTerms' ([], [], as, bs)

            dropEqualTerms' (as', bs', queueA, queueB) = case (PQueue.maxViewWithKey queueA, PQueue.maxViewWithKey queueB) of
              (Just ((kA, a), queueA'), Just ((kB, b), queueB')) ->
                if hashInt a /= hashInt b
                then dropEqualTerms' (toUnmappedTerm a : as', toUnmappedTerm b : bs', queueA', queueB')
                else dropEqualTerms' (as', bs', queueA', queueB')
              (Just ((_, a), queueA'), Nothing) -> (toUnmappedTerm a : as', bs', queueA', queueB)
              (Nothing, Just ((_, b), queueB')) -> (as', toUnmappedTerm b : bs', queueA, queueB')
              (Nothing, Nothing) -> (as', bs', queueA, queueB)
            toUnmappedTerm (UnmappedHashTerm index _ term) = UnmappedTerm index (getField (extract term)) term

        hashabilize :: (HasField fields Int) => Int -> Cofree f (Record fields) -> (Int, UnmappedHashTerm (Cofree f (Record fields)))
        hashabilize index term = (termSize term, UnmappedHashTerm index hash term)
          where hash = (getField (extract term) :: Int)

        kdas = KdTree.build (Vector.toList . feature) fas
        kdbs = KdTree.build (Vector.toList . feature) fbs
        featurize index term = UnmappedTerm index (getField (extract term)) term
        toMap = IntMap.fromList . fmap (termIndex &&& identity)
        -- | Construct a diff for a term in B by matching it against the most similar eligible term in A (if any), marking both as ineligible for future matches.
        findNearestNeighbourTo :: UnmappedTerm (Cofree f (Record fields)) -> State (Int, IntMap (UnmappedTerm (Cofree f (Record fields))), IntMap (UnmappedTerm (Cofree f (Record fields)))) (Int, Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields))))
        findNearestNeighbourTo kv@(UnmappedTerm j _ b) = do
          (previous, unmappedA, unmappedB) <- get
          fromMaybe (insertion previous unmappedA unmappedB kv) $ do
            foundA@(UnmappedTerm i _ a) <- nearestUnmapped (IntMap.filterWithKey (\ k _ -> isInMoveBounds previous k) unmappedA) kdas kv
            UnmappedTerm j' _ _ <- nearestUnmapped unmappedB kdbs foundA
            guard (j == j')
            compared <- compare a b
            pure $! do
              put (i, IntMap.delete i unmappedA, IntMap.delete j unmappedB)
              pure (i, compared)

        -- | Determines whether an index is in-bounds for a move given the most recently matched index.
        isInMoveBounds previous i = previous <= i && i <= previous + defaultMoveBound

        -- | Finds the most-similar unmapped term to the passed-in term, if any.
        --
        -- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which have already been mapped, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
        --
        -- cf §4.2 of RWS-Diff
        nearestUnmapped
          :: IntMap (UnmappedTerm (Cofree f (Record fields))) -- ^ A set of terms eligible for matching against.
          -> KdTree.KdTree Double (UnmappedTerm (Cofree f (Record fields))) -- ^ The k-d tree to look up nearest neighbours within.
          -> UnmappedTerm (Cofree f (Record fields)) -- ^ The term to find the nearest neighbour to.
          -> Maybe (UnmappedTerm (Cofree f (Record fields))) -- ^ The most similar unmapped term, if any.
        nearestUnmapped unmapped tree key = getFirst $ foldMap (First . Just) (sortOn (maybe maxBound (editDistanceUpTo defaultM) . compare (term key) . term) (toList (IntMap.intersection unmapped (toMap (KdTree.kNearest tree defaultL key)))))

        insertion previous unmappedA unmappedB (UnmappedTerm j _ b) = do
          put (previous, unmappedA, IntMap.delete j unmappedB)
          pure (negate 1, inserting b)

        -- Given a list of diffs, and unmapped terms in unmappedA, deletes
        -- any terms that remain in umappedA.
        deleteRemaining diffs (_, unmappedA, _) = foldl' (flip (List.insertBy (comparing fst))) diffs ((termIndex &&& deleting . term) <$> unmappedA)

-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (Prologue.Foldable f, Functor f) => Integer -> Free (CofreeF f (Both a)) (Patch (Cofree f a)) -> Int
editDistanceUpTo m = diffSum (patchSum termSize) . cutoff m
  where diffSum patchCost diff = sum $ fmap (maybe 0 patchCost) diff

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
data UnmappedTerm a = UnmappedTerm { termIndex :: {-# UNPACK #-} !Int, feature :: !(Vector.Vector Double), term :: !a }
  deriving Eq

data UnmappedHashTerm a = UnmappedHashTerm { hashTermIndex :: {-# UNPACK #-} !Int, hashInt :: !Int, hashTerm :: !a }
  deriving Eq


-- | A `Gram` is a fixed-size view of some portion of a tree, consisting of a `stem` of _p_ labels for parent nodes, and a `base` of _q_ labels of sibling nodes. Collectively, the bag of `Gram`s for each node of a tree (e.g. as computed by `pqGrams`) form a summary of the tree.
data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

-- -- | Annotates a term with it's hash at each node.
hashDecorator :: forall hash f fields. (Hashable (f Int), Hashable hash, Traversable f) => (forall b. CofreeF f (Record fields) b -> hash) -> Cofree f (Record fields) -> Cofree f (Record (Int ': fields))
hashDecorator getLabel = cata $ \case
  term@(record :< functor) ->
    cofree ((hash (getLabel term, fmap (getField . extract) functor :: f Int) .: record) :< functor)

-- | Annotates a term with a feature vector at each node, using the default values for the p, q, and d parameters.
defaultFeatureVectorDecorator :: (Hashable label, Traversable f) => (forall b. CofreeF f (Record fields) b -> label) -> Cofree f (Record fields) -> Cofree f (Record (Vector.Vector Double ': fields))
defaultFeatureVectorDecorator getLabel = featureVectorDecorator getLabel defaultP defaultQ defaultD

-- | Annotates a term with a feature vector at each node, parameterized by stem length, base width, and feature vector dimensions.
featureVectorDecorator :: (Hashable label, Traversable f) => (forall b. CofreeF f (Record fields) b -> label) -> Int -> Int -> Int -> Cofree f (Record fields) -> Cofree f (Record (Vector.Vector Double ': fields))
featureVectorDecorator getLabel p q d
  = cata (\ (RCons gram rest :< functor) ->
      cofree ((foldr (Vector.zipWith (+) . getField . extract) (unitVector d (hash gram)) functor .: rest) :< functor))
  . pqGramDecorator getLabel p q

-- | Annotates a term with the corresponding p,q-gram at each node.
pqGramDecorator :: Traversable f
  => (forall b. CofreeF f (Record fields) b -> label) -- ^ A function computing the label from an arbitrary unpacked term. This function can use the annotation and functor’s constructor, but not any recursive values inside the functor (since they’re held parametric in 'b').
  -> Int -- ^ 'p'; the desired stem length for the grams.
  -> Int -- ^ 'q'; the desired base length for the grams.
  -> Cofree f (Record fields) -- ^ The term to decorate.
  -> Cofree f (Record (Gram label ': fields)) -- ^ The decorated term.
pqGramDecorator getLabel p q = cata algebra
  where algebra term = let label = getLabel term in
          cofree ((gram label .: headF term) :< assignParentAndSiblingLabels (tailF term) label)
        gram label = Gram (padToSize p []) (padToSize q (pure (Just label)))
        assignParentAndSiblingLabels functor label = (`evalState` (replicate (q `div` 2) Nothing <> siblingLabels functor)) (for functor (assignLabels label))

        assignLabels :: label -> Cofree f (Record (Gram label ': fields)) -> State [Maybe label] (Cofree f (Record (Gram label ': fields)))
        assignLabels label a = case runCofree a of
          RCons gram rest :< functor -> do
            labels <- get
            put (drop 1 labels)
            pure $! cofree ((gram { stem = padToSize p (Just label : stem gram), base = padToSize q labels } .: rest) :< functor)
        siblingLabels :: Traversable f => f (Cofree f (Record (Gram label ': fields))) -> [Maybe label]
        siblingLabels = foldMap (base . rhead . extract)
        padToSize n list = take n (list <> repeat empty)

-- | Computes a unit vector of the specified dimension from a hash.
unitVector :: Int -> Int -> Vector.Vector Double
unitVector d hash = normalize ((`evalRand` mkQCGen hash) (sequenceA (Vector.replicate d getRandom)))
  where normalize vec = fmap (/ vmagnitude vec) vec
        vmagnitude = sqrtDouble . Vector.sum . fmap (** 2)

-- | Strips the head annotation off a term annotated with non-empty records.
stripTerm :: Functor f => Cofree f (Record (h ': t)) -> Cofree f (Record t)
stripTerm = fmap rtail

-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: (Functor f, Functor g) => Free (CofreeF f (g (Record (h ': t)))) (Patch (Cofree f (Record (h ': t)))) -> Free (CofreeF f (g (Record t))) (Patch (Cofree f (Record t)))
stripDiff = iter (\ (h :< f) -> wrap (fmap rtail h :< f)) . fmap (pure . fmap stripTerm)


-- Instances

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash gram = hash (stem gram <> base gram)

-- | Construct a generator for arbitrary `Gram`s of size `(p, q)`.
gramWithPQ :: Arbitrary label => Int -> Int -> Gen (Gram label)
gramWithPQ p q = Gram <$> vectorOf p arbitrary <*> vectorOf q arbitrary

instance Arbitrary label => Arbitrary (Gram label) where
  arbitrary = join $ gramWithPQ <$> arbitrary <*> arbitrary

  shrink (Gram a b) = Gram <$> shrink a <*> shrink b
