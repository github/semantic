module Data.RandomWalkSimilarity where

import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad.State
import qualified Data.DList as DList
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.KdTree.Static as KdTree
import qualified Data.List as List
import qualified Data.OrderedMap as Map
import qualified Data.Vector as Vector
import Diff
import Patch
import Prologue
import Syntax
import Term
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Random

-- | Given a function comparing two terms recursively, and a function to compute a Hashable label from an annotation, compute the diff of a pair of lists of terms using a random walk similarity metric, which completes in log-linear time. This implementation is based on the paper [_RWS-Diff—Flexible and Efficient Change Detection in Hierarchical Data_](https://github.com/github/semantic-diff/files/325837/RWS-Diff.Flexible.and.Efficient.Change.Detection.in.Hierarchical.Data.pdf).
rws :: (Hashable label, Hashable leaf, Eq leaf, Eq annotation) => (Term leaf annotation -> Term leaf annotation -> Maybe (Diff leaf annotation)) -> (annotation -> label) -> [Term leaf annotation] -> [Term leaf annotation] -> [Diff leaf annotation]
rws compare getLabel as bs
  | null as, null bs = []
  | null as = insert <$> bs
  | null bs = delete <$> as
  | otherwise = fmap snd . uncurry deleteRemaining . (`runState` (negate 1, fas)) $ traverse findNearestNeighbourTo fbs
  where insert = pure . Insert
        delete = pure . Delete
        (p, q, d) = (2, 2, 15)
        fas = zipWith featurize [0..] as
        fbs = zipWith featurize [0..] bs
        kdas = KdTree.build (Vector.toList . feature) fas
        featurize index term = UnmappedTerm index (featureVector d (pqGrams p q getLabel term)) term
        findNearestNeighbourTo kv@(UnmappedTerm _ _ v) = do
          (previous, unmapped) <- get
          let (UnmappedTerm i k _) = KdTree.nearest kdas kv
          fromMaybe (pure [ (negate 1, insert v) ]) $ do
            UnmappedTerm _ _ found <- find ((== k) . feature) unmapped
            guard (i < previous)
            compared <- compare found v
            pure $! do
              put (i, List.delete (UnmappedTerm i k found) unmapped)
              pure [ (i, compared) ]
        deleteRemaining diffs (_, unmapped) = foldl' (flip (List.insertBy (comparing fst))) (join diffs) ((termIndex &&& delete . term) <$> unmapped)

data UnmappedTerm leaf annotation = UnmappedTerm { termIndex :: {-# UNPACK #-} !Int, feature :: !(Vector.Vector Double), term :: !(Term leaf annotation) }
  deriving Eq

longestIncreasingSubsequence :: Ord a => [a] -> [a]
longestIncreasingSubsequence = longestOrderedSubsequenceBy (<)

longestOrderedSubsequenceBy :: (a -> a -> Bool) -> [a] -> [a]
longestOrderedSubsequenceBy isOrd = maximumBy (comparing length) . filter (isSortedBy isOrd) . subsequences

isSorted :: Ord a => [a] -> Bool
isSorted = isSortedBy (<=)

isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy isOrd = para $ \ l -> case l of
  Cons h (tail, tailIsSorted) -> tailIsSorted && maybe True (isOrd h) (head tail)
  Nil -> True

-- | A `Gram` is a fixed-size view of some portion of a tree, consisting of a `stem` of _p_ labels for parent nodes, and a `base` of _q_ labels of sibling nodes. Collectively, the bag of `Gram`s for each node of a tree (e.g. as computed by `pqGrams`) form a summary of the tree.
data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

-- | Compute the bag of grams with stems of length _p_ and bases of length _q_, with labels computed from annotations, which summarize the entire subtree of a term.
pqGrams :: Int -> Int -> (annotation -> label) -> Cofree (Syntax leaf) annotation -> DList.DList (Gram (label, Maybe leaf))
pqGrams p q getLabel = cata merge . setRootBase . setRootStem . hylo go project
  where go (annotation :< functor) = cofree (Gram [] [ Just (getLabel annotation, leafValue functor) ] :< (assignParent (Just (getLabel annotation, leafValue functor)) p <$> functor))
        leafValue (Leaf s) = Just s
        leafValue _ = Nothing
        merge (head :< tail) = DList.cons head (Prologue.fold tail)
        assignParent parentLabel n tree
          | n > 0 = let gram :< functor = runCofree tree in cofree $ prependParent parentLabel gram :< assignSiblings (assignParent parentLabel (pred n) <$> functor)
          | otherwise = tree
        prependParent parentLabel gram = gram { stem = parentLabel : stem gram }
        assignSiblings functor = case functor of
          Leaf a -> Leaf a
          Indexed a -> Indexed $ windowed q setBases [] a
          Fixed a -> Fixed $ windowed q setBases [] a
          Keyed a -> Keyed . Map.fromList $ windowed q setBasesKV [] (Map.toList a)
        setBases child siblings rest = let (gram :< further) = (runCofree child) in cofree (setBase gram (siblings >>= base . extract) :< further) : rest
        setBasesKV (key, child) siblings rest = let (gram :< further) = (runCofree child) in (key, cofree (setBase gram (siblings >>= base . extract . snd) :< further)) : rest
        setBase gram newBase = gram { base = take q (newBase <> repeat Nothing) }
        setRootBase term = let (a :< f) = runCofree term in cofree (setBase a (base a) :< f)
        setRootStem = foldr (\ p rest -> assignParent Nothing p . rest) identity [0..p]

-- | A sliding-window fold over _n_ items of a list per iteration.
windowed :: Int -> (a -> [a] -> b -> b) -> b -> [a] -> b
windowed n f seed = para alg
  where alg xs = case xs of
          Cons a (as, b) -> f a (take n $ a : as) b
          Nil -> seed


-- | Compute a vector with the specified number of dimensions, as an approximation of a bag of `Gram`s summarizing a tree.
featureVector :: Hashable label => Int -> DList.DList (Gram label) -> Vector.Vector Double
featureVector d bag = sumVectors $ unitDVector . hash <$> bag
  where unitDVector hash = normalize . (`evalRand` mkQCGen hash) $ Prologue.sequence (Vector.replicate d getRandom)
        normalize vec = fmap (/ vmagnitude vec) vec
        sumVectors = DList.foldr (Vector.zipWith (+)) (Vector.replicate d 0)

-- | The magnitude of a Euclidean vector, i.e. its distance from the origin.
vmagnitude :: Vector.Vector Double -> Double
vmagnitude = sqrtDouble . Vector.sum . fmap (** 2)


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
