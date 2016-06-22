module Data.Gram where

import Control.Monad.Random
import qualified Data.DList as DList
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.Vector as Vector
import Prologue
import Syntax
import Term ()
import Test.QuickCheck.Random

data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

serialize :: Gram label -> [Maybe label]
serialize gram = stem gram <> base gram

pqGrams :: Int -> Int -> Cofree (Syntax leaf) label -> Bag (Gram label)
pqGrams p q = cata merge . (\ t -> let (a :< f) = runCofree t in cofree (setBase a (base a) :< f)) . foldr (\ p rest -> assignParent Nothing p . rest) identity [0..p] . hylo go project
  where go (label :< functor) = cofree (Gram [] [ Just label ] :< (assignParent (Just label) p <$> functor))
        merge (head :< tail) = DList.singleton head <> Prologue.fold tail
        assignParent parentLabel n tree
          | n > 0 = let gram :< functor = runCofree tree in cofree $ prependParent parentLabel gram :< assignSiblings (assignParent parentLabel (pred n) <$> functor)
          | otherwise = tree
        prependParent parentLabel gram = gram { stem = parentLabel : stem gram }
        assignSiblings functor = case functor of
          Leaf a -> Leaf a
          Indexed a -> Indexed $ windowed q setBases [] a
          Fixed a -> Fixed $ windowed q setBases [] a
          Keyed a -> Keyed a
        setBases child siblings rest = let (gram :< further) = (runCofree child) in cofree (setBase gram (siblings >>= base . extract) :< further) : rest
        setBase gram newBase = gram { base = take q (newBase <> repeat Nothing) }

windowed :: Int -> (a -> [a] -> b -> b) -> b -> [a] -> b
windowed n f seed = para alg
  where alg xs = case xs of
          Cons a (as, b) -> f a (take n $ a : as) b
          Nil -> seed

type Bag = DList.DList


featureVector :: Hashable label => Bag (Gram label) -> Int -> Vector.Vector Double
featureVector bag d = sumVectors $ unitDVector . hash <$> bag
  where unitDVector hash = normalize . (`evalRand` mkQCGen hash) $ Prologue.sequence (Vector.replicate d getRandom)
        normalize vec = fmap (/ magnitude vec) vec
        magnitude vec = sqrtDouble (Vector.sum (fmap (** 2) vec))
        sumVectors = DList.foldr (Vector.zipWith (+)) (Vector.replicate d 0)

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash = hash . serialize
