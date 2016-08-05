{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators #-}
module Data.RandomWalkSimilarity
( rws
, pqGrams
, featureVector
, featureVectorDecorator
, Gram(..)
) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad.State
import qualified Data.DList as DList
import Data.Functor.Both hiding (fst, snd)
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.KdTree.Static as KdTree
import qualified Data.List as List
import Data.Record
import qualified Data.Vector as Vector
import Patch
import Prologue
import Term ()
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Random

-- | Given a function comparing two terms recursively, and a function to compute a Hashable label from an unpacked term, compute the diff of a pair of lists of terms using a random walk similarity metric, which completes in log-linear time. This implementation is based on the paper [_RWS-Diff—Flexible and Efficient Change Detection in Hierarchical Data_](https://github.com/github/semantic-diff/files/325837/RWS-Diff.Flexible.and.Efficient.Change.Detection.in.Hierarchical.Data.pdf).
rws :: (Eq (Record fields), Prologue.Foldable f, Functor f, Eq (f (Cofree f (Record fields))), HasField fields (Vector.Vector Double)) =>
  -- | A function which comapres a pair of terms recursively, returning 'Just' their diffed value if appropriate, or 'Nothing' if they should not be compared.
  (Cofree f (Record fields) -> Cofree f (Record fields) -> Maybe (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields))))) ->
  -- | The old list of terms.
  [Cofree f (Record fields)] ->
  -- | The new list of terms.
  [Cofree f (Record fields)] ->
  [Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))]
rws compare as bs
  | null as, null bs = []
  | null as = insert <$> bs
  | null bs = delete <$> as
  | otherwise = fmap snd . uncurry deleteRemaining . (`runState` (negate 1, fas)) $ traverse findNearestNeighbourTo fbs
  where insert = pure . Insert
        delete = pure . Delete
        fas = zipWith featurize [0..] as
        fbs = zipWith featurize [0..] bs
        kdas = KdTree.build (Vector.toList . feature) fas
        featurize index term = UnmappedTerm index (getField (extract term)) term
        findNearestNeighbourTo kv@(UnmappedTerm _ _ v) = do
          (previous, unmapped) <- get
          let (UnmappedTerm i _ _) = KdTree.nearest kdas kv
          fromMaybe (pure (negate 1, insert v)) $ do
            found <- find ((== i) . termIndex) unmapped
            guard (i >= previous)
            compared <- compare (term found) v
            pure $! do
              put (i, List.delete found unmapped)
              pure (i, compared)
        deleteRemaining diffs (_, unmapped) = foldl' (flip (List.insertBy (comparing fst))) diffs ((termIndex &&& delete . term) <$> unmapped)

-- | A term which has not yet been mapped by `rws`, along with its feature vector summary & index.
data UnmappedTerm a = UnmappedTerm { termIndex :: {-# UNPACK #-} !Int, feature :: !(Vector.Vector Double), term :: !a }
  deriving Eq


-- | A `Gram` is a fixed-size view of some portion of a tree, consisting of a `stem` of _p_ labels for parent nodes, and a `base` of _q_ labels of sibling nodes. Collectively, the bag of `Gram`s for each node of a tree (e.g. as computed by `pqGrams`) form a summary of the tree.
data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

-- | Compute the bag of grams with stems of length _p_ and bases of length _q_, with labels computed from annotations, which summarize the entire subtree of a term.
pqGrams :: (Prologue.Foldable f, Functor f) => (forall b. CofreeF f (Record fields) b -> label) -> Int -> Int -> Cofree f (Record fields) -> DList.DList (Gram label)
pqGrams getLabel p q = getField . extract . decorateTermWithBagOfPQGrams q . decorateTermWithPGram p . decorateTermWithLabel getLabel


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

decorateTermWithLabel :: Functor f => (forall b. CofreeF f (Record fields) b -> label) -> Cofree f (Record fields) -> Cofree f (Record (label ': fields))
decorateTermWithLabel getLabel = cata $ \ c -> cofree ((getLabel c .: headF c) :< tailF c)

decorateTermWithPGram :: Functor f => Int -> Cofree f (Record (label ': fields)) -> Cofree f (Record (Gram label ': fields))
decorateTermWithPGram p = ana coalgebra . (,) []
  where coalgebra :: Functor f => ([Maybe label], Cofree f (Record (label ': fields))) -> CofreeF f (Record (Gram label ': fields)) ([Maybe label], Cofree f (Record (label ': fields)))
        coalgebra (parentLabels, c) = case extract c of
          RCons label rest -> (Gram (padToSize p parentLabels) (pure (Just label)) .: rest) :< fmap ((,) (padToSize p (Just label : parentLabels))) (unwrap c)

decorateTermWithBagOfPQGrams :: (Prologue.Foldable f, Functor f) => Int -> Cofree f (Record (Gram label ': fields)) -> Cofree f (Record (DList.DList (Gram label) ': fields))
decorateTermWithBagOfPQGrams q = fmap (\ (RCons (first, rest) t) -> DList.cons (first { base = padToSize q (base first) }) rest .: t) . cata algebra
  where algebra :: (Prologue.Foldable f, Functor f) => CofreeF f (Record (Gram label ': fields)) (Cofree f (Record ((Gram label, DList.DList (Gram label)) ': fields))) -> Cofree f (Record ((Gram label, DList.DList (Gram label)) ': fields))
        algebra (RCons gram rest :< functor) = cofree (((gram, DList.fromList (windowed q setBases [] (fst . getGrams . extract <$> toList functor)) <> foldMap (snd . getGrams . extract) functor) .: rest) :< functor)
        setBases :: Gram label -> [Gram label] -> [Gram label] -> [Gram label]
        setBases gram siblings rest = gram { base = padToSize q (foldMap base siblings) } : rest
        getGrams :: HasField fields (Gram label, DList.DList (Gram label)) => Record fields -> (Gram label, DList.DList (Gram label))
        getGrams = getField

decorateTermWithFeatureVector :: (Hashable label, Functor f) => Int -> Cofree f (Record (DList.DList (Gram label) ': fields)) -> Cofree f (Record (Vector.Vector Double ': fields))
decorateTermWithFeatureVector d = fmap $ \ (RCons grams rest) -> featureVector d grams .: rest

featureVectorDecorator :: (Hashable label, Functor f, Prologue.Foldable f) => (forall b. CofreeF f (Record fields) b -> label) -> Int -> Int -> Int -> Cofree f (Record fields) -> Cofree f (Record (Vector.Vector Double ': fields))
featureVectorDecorator getLabel p q d
  = decorateTermWithFeatureVector d
  . decorateTermWithBagOfPQGrams q
  . decorateTermWithPGram p
  . decorateTermWithLabel getLabel

-- | Pads a list of Alternative values to exactly n elements.
padToSize :: Alternative f => Int -> [f a] -> [f a]
padToSize n list = take n (list <> repeat empty)

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
