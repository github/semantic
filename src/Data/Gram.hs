{-# LANGUAGE RankNTypes #-}
module Data.Gram where

import Control.Monad.Random
import qualified Data.DList as DList
import Data.Functor.Foldable as Foldable
import Data.Hashable
import qualified Data.Vector as Vector
import Prologue
import Test.QuickCheck.Random

data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
  deriving (Eq, Show)

serialize :: Gram label -> [Maybe label]
serialize gram = stem gram <> base gram

pqGrams :: Foldable.Foldable tree => Int -> Int -> (forall a. Base tree a -> (label, [a])) -> tree -> Bag (Gram label)
pqGrams p q unpack = foldr (<>) empty . assignParent Nothing p . cata go
  where go functor = let (label, children) = unpack functor in
          DList.singleton (Gram [] [ Just label ]) : (children >>= assignParent (Just label) p)
        assignParent parentLabel n children
          | n == 0 = children
          | otherwise = case children of
            (head : tail) -> (prependParent parentLabel <$> head) : assignParent parentLabel (pred n) tail
            [] -> []
        prependParent parentLabel gram = gram { stem = parentLabel : stem gram }

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
