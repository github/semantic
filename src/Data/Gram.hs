{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Data.Gram where

import Control.Monad.Random
import qualified Data.DList as DList
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.Vector as Vector
import Prologue
import Term ()
import Test.QuickCheck.Random

data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

serialize :: Gram label -> [Maybe label]
serialize gram = stem gram <> base gram

pqGrams :: forall label tree. (Foldable.Foldable tree, Prologue.Foldable (Base tree)) => Int -> Int -> (forall a. Base tree a -> label) -> tree -> Bag (Gram label)
pqGrams p q getLabel = cata merge . foldr (\ p rest -> assignParent Nothing p . rest) identity [0..p] . hylo go project
  where go functor = let label = getLabel functor in
          cofree (DList.singleton (Gram [] [ Just label ]) :< (assignParent (Just label) p <$> functor))
        merge (head :< tail) = head <> Prologue.fold tail
        assignParent parentLabel n tree
          | n > 0 = let gram :< functor = runCofree tree in cofree $ (prependParent parentLabel <$> gram) :< (assignParent parentLabel (pred n) <$> functor)
          | otherwise = tree
        prependParent parentLabel gram = gram { stem = parentLabel : stem gram }

windowed :: forall a b. Int -> ([a] -> b -> b) -> b -> [a] -> b
windowed n f seed = para alg
  where alg :: Prim [a] ([a], b) -> b
        alg xs = case xs of
          Cons a (as, b) -> f (take n $ a : as) b
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
