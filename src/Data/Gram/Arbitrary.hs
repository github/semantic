{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Gram.Arbitrary where

import Data.Gram
import Prologue
import Test.QuickCheck

gramWithPQ :: Arbitrary label => Int -> Int -> Gen (Gram label)
gramWithPQ p q = Gram <$> vectorOf p arbitrary <*> vectorOf q arbitrary

instance Arbitrary label => Arbitrary (Gram label) where
  arbitrary = join $ gramWithPQ <$> arbitrary <*> arbitrary

  shrink (Gram a b) = Gram <$> shrink a <*> shrink b
