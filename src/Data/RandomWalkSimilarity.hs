module Data.RandomWalkSimilarity where

import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad.State
import Data.Bifunctor.Join
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

rws :: (Hashable label, Hashable leaf, Eq leaf, Ord annotation) => (Term leaf annotation -> Term leaf annotation -> Maybe (Diff leaf annotation)) -> (annotation -> label) -> [Term leaf annotation] -> [Term leaf annotation] -> [Diff leaf annotation]
rws compare getLabel as bs
  | null as, null bs = []
  | null as = insert <$> bs
  | null bs = delete <$> as
  | otherwise = uncurry deleteRemaining . (`runState` fas) $ traverse findNearestNeighbourTo fbs
  where insert = pure . Insert
        delete = pure . Delete
        replace = (pure .) . Replace
        (p, q, d) = (2, 2, 15)
        fas = featurize <$> as
        fbs = featurize <$> bs
        kdas = KdTree.build (Vector.toList . fst) fas
        featurize = featureVector d . pqGrams p q getLabel &&& identity
        findNearestNeighbourTo kv@(_, v) = do
          unmapped <- get
          let (k, _) = KdTree.nearest kdas kv
          case k `List.lookup` unmapped of
            Nothing -> pure $! insert v
            Just found -> do
              put (List.delete (k, found) unmapped)
              pure $! fromMaybe (replace found v) (compare found v)
        deleteRemaining diffs unmapped = foldl' (flip (List.insertBy (comparing firstAnnotation))) diffs (delete . snd <$> unmapped)

-- | Extract the annotation for the before state of a diff node. This is returned in `Maybe` because e.g. an `Insert` patch does not have an annotation for the before state.
firstAnnotation :: Diff leaf annotation -> Maybe annotation
firstAnnotation diff = case runFree diff of
  Free (annotations :< _) -> Just (fst (runJoin annotations))
  Pure patch -> maybeFst (unPatch $ extract <$> patch)

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
