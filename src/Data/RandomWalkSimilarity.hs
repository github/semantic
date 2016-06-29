module Data.RandomWalkSimilarity where

import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad.State
import qualified Data.DList as DList
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.KdTree.Static as KdTree
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Diff
import Patch
import Prologue
import Syntax
import Term
import Test.QuickCheck.Random

rws :: Hashable label => (Term leaf annotation -> Term leaf annotation -> Maybe (Diff leaf annotation)) -> (annotation -> label) -> [Term leaf annotation] -> [Term leaf annotation] -> [Diff leaf annotation]
rws compare getLabel as bs
  | null as, null bs = []
  | null as = insert <$> bs
  | null bs = delete <$> as
  | otherwise = fmap fst . uncurry deleteRemaining . (`runState` Set.empty) $ traverse findNearestNeighbourTo fbs
  where insert = pure . Insert
        delete = pure . Delete
        replace = (pure .) . Replace
        (p, q) = (2, 2)
        d = 15
        fas = zip (featurize <$> as) [0..]
        fbs = zip (featurize <$> bs) [0..]
        kdas = KdTree.build (Vector.toList . fst . fst) fas
        featurize = featureVector d . pqGrams p q getLabel &&& identity
        findNearestNeighbourTo kv@((_, v), i) = do
          mapped <- get
          let ((k, nearest), j) = KdTree.nearest kdas kv
          if k `Set.member` mapped
            then pure (insert v, j)
            else do
              put (Set.insert k mapped)
              pure $! maybe (replace nearest v, j) (flip (,) j) (compare nearest v)
        deleteRemaining diff mapped = diff <> (first (delete . snd) <$> filter (not . (`Set.member` mapped) . fst . fst) fas)

data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

serialize :: Gram label -> [Maybe label]
serialize gram = stem gram <> base gram

pqGrams :: Int -> Int -> (annotation -> label) -> Cofree (Syntax leaf) annotation -> Bag (Gram label)
pqGrams p q getLabel = cata merge . setRootBase . setRootStem . hylo go project
  where go (annotation :< functor) = cofree (Gram [] [ Just (getLabel annotation) ] :< (assignParent (Just (getLabel annotation)) p <$> functor))
        merge (head :< tail) = DList.singleton head <> Prologue.fold tail
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

windowed :: Int -> (a -> [a] -> b -> b) -> b -> [a] -> b
windowed n f seed = para alg
  where alg xs = case xs of
          Cons a (as, b) -> f a (take n $ a : as) b
          Nil -> seed

type Bag = DList.DList


featureVector :: Hashable label => Int -> Bag (Gram label) -> Vector.Vector Double
featureVector d bag = sumVectors $ unitDVector . hash <$> bag
  where unitDVector hash = normalize . (`evalRand` mkQCGen hash) $ Prologue.sequence (Vector.replicate d getRandom)
        normalize vec = fmap (/ vmagnitude vec) vec
        sumVectors = DList.foldr (Vector.zipWith (+)) (Vector.replicate d 0)

vmagnitude :: Vector.Vector Double -> Double
vmagnitude vec = sqrtDouble (Vector.sum (fmap (** 2) vec))

nearestNeighbour :: [(Vector.Vector Double, a)] -> Vector.Vector Double -> Maybe a
nearestNeighbour [] _ = Nothing
nearestNeighbour children v = Just . snd $ minimumBy (compare `on` distance v . fst) children
  where distance a b = vmagnitude $ Vector.zipWith (-) a b

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash = hash . serialize
