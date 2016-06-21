module Data.Gram where

import Control.Monad.Random
import Data.DList as DList
import Data.Hashable
import Data.Vector as Vector
import Prologue
import Test.QuickCheck.Random

data Gram label = Gram { stem :: [label], base :: [label] }

serialize :: Gram label -> [label]
serialize gram = stem gram <> base gram


type Bag = DList


featureVector :: Hashable label => Bag (Gram label) -> Int -> Vector Rational
featureVector bag d = sumVectors $ unitDVector . hash <$> bag
  where unitDVector hash = normalize . (`evalRand` mkQCGen hash) $ Prologue.sequence (Vector.replicate d getRandom)
        normalize vec = fmap (/ magnitude vec) vec
        magnitude vec = toRational (sqrtDouble (fromRational (Vector.sum (fmap (^^ (2 :: Integer)) vec))))
        sumVectors = DList.foldr (Vector.zipWith (+)) (Vector.replicate d 0)

instance Hashable label => Hashable (Gram label) where
  hashWithSalt _ = hash
  hash = hash . serialize

instance (Random a, Integral a) => Random (Ratio a) where
  random = first (% 1) . random
