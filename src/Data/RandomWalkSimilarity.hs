{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators #-}
module Data.RandomWalkSimilarity
( rws
, pqGrams
, featureVector
, Gram(..)
) where

import Control.Arrow ((&&&))
import qualified Control.Monad.Free as Free (Free)
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
rws :: (Hashable label, Eq annotation, Prologue.Foldable f, Functor f, Eq (f (Cofree f annotation))) =>
  -- | A function which comapres a pair of terms recursively, returning 'Just' their diffed value if appropriate, or 'Nothing' if they should not be compared.
  (Cofree f annotation -> Cofree f annotation -> Maybe (Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation)))) ->
  -- | A function to compute a label for an unpacked term.
  (forall b. CofreeF f annotation b -> label) ->
  -- | The old list of terms.
  [Cofree f annotation] ->
  -- | The new list of terms.
  [Cofree f annotation] ->
  [Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation))]
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
        featurize index term = UnmappedTerm index (featureVector d (pqGrams getLabel p q term)) term
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
pqGrams :: (Prologue.Foldable f, Functor f) => (forall b. CofreeF f annotation b -> label) -> Int -> Int -> Cofree f annotation -> DList.DList (Gram label)
pqGrams getLabel p q = uncurry DList.cons . cata merge . setRootBase . setRootStem . cata go
  where go c = cofree (Gram [] [ Just (getLabel c) ] :< (assignParent (Just (getLabel c)) p <$> tailF c))
        merge (head :< tail) = let tail' = toList tail in (head, DList.fromList (windowed q setBases [] (fst <$> tail')) <> foldMap snd tail')
        assignParent parentLabel n tree
          | n > 0 = let gram :< functor = runCofree tree in cofree $ prependParent parentLabel gram :< (assignParent parentLabel (pred n) <$> functor)
          | otherwise = tree
        prependParent parentLabel gram = gram { stem = parentLabel : stem gram }
        setBases gram siblings rest = setBase gram (foldMap base siblings) : rest
        setBase gram newBase = gram { base = take q (newBase <> repeat Nothing) }
        setRootBase term = let (a :< f) = runCofree term in cofree (setBase a (base a) :< f)
        setRootStem = foldr (\ p rest -> assignParent Nothing p . rest) identity [0..p]

type TermDecorator f fields field = CofreeF f (Record fields) (Record (field ': fields)) -> field

pqGramDecorator :: (Prologue.Foldable f, Functor f) => (forall b. CofreeF f (Record a) b -> label) -> Int -> Int -> TermDecorator f a (Gram label, DList.DList (Gram label))
pqGramDecorator getLabel p q c@(a :< s) = (Gram [] [ Just label ], foldMap (childGrams label) s)
  where childGrams :: HasField fields (Gram label, DList.DList (Gram label)) => label -> Record fields -> DList.DList (Gram label)
        childGrams label record = let (child, grandchildren) = getField record in
          DList.singleton (prependParent label child) <> grandchildren
        prependParent label gram = gram { stem = Just label : stem gram }
        label = getLabel c


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

featureVectorDecorator :: (Prologue.Foldable f, Functor f) => (forall b. CofreeF f (Record a) b -> label) -> Int -> Int -> Int -> TermDecorator f a (Vector.Vector Double)
featureVectorDecorator getLabel p q d (a :< s) = Vector.replicate d 0

decorateTermWithLabel :: (Typeable label, Functor f) => (forall b. CofreeF f (Record fields) b -> label) -> Cofree f (Record fields) -> Cofree f (Record (label ': fields))
decorateTermWithLabel getLabel = cata $ \ c -> cofree ((getLabel c .: headF c) :< tailF c)

decorateTermWithPQGram :: (Typeable label, Functor f) => Int -> Int -> Cofree f (Record (label ': fields)) -> Cofree f (Record (Gram label ': fields))
decorateTermWithPQGram p q = futu coalgebra . (,) []
  where coalgebra :: Functor f => ([Maybe label], Cofree f (Record (label ': fields))) -> CofreeF f (Record (Gram label ': fields)) (Free.Free (CofreeF f (Record (Gram label ': fields))) ([Maybe label], Cofree f (Record (label ': fields))))
        coalgebra (parentLabels, c) = case extract c of
          RCons label rest -> (Gram (take p (parentLabels <> repeat Nothing)) (pure (Just label)) .: rest) :< fmap (pure . (,) (take p (Just label : parentLabels))) (unwrap c)

decorateTermWithBagsOfPQGrams :: (Prologue.Foldable f, Functor f) => Cofree f (Record (Gram label ': fields)) -> Cofree f (Record (DList.DList (Gram label) ': fields))
decorateTermWithBagsOfPQGrams = cata $ \ (RCons gram rest :< functor) -> cofree ((DList.cons gram (foldMap (getField . extract) functor) .: rest) :< functor)

decorateTermWithFeatureVector :: (Hashable label, Functor f) => Int -> Cofree f (Record (DList.DList (Gram label) ': fields)) -> Cofree f (Record (Vector.Vector Double ': fields))
decorateTermWithFeatureVector d = fmap $ \ (RCons grams rest) -> featureVector d grams .: rest

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
