{-# LANGUAGE DeriveAnyClass, FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Alignment
import Criterion.Main
import Data.Bifunctor.Join
import Data.String
import Data.These
import Prologue
import Test.QuickCheck

main :: IO ()
main = do
  benchmarks <- sequenceA [ generativeBenchmark "numberedRows" length (nf (numberedRows :: [Join These ()] -> [Join These (Int, ())])) ]
  defaultMain benchmarks

generativeBenchmark :: (Arbitrary a, Show m) => String -> (a -> m) -> (a -> Benchmarkable) -> IO Benchmark
generativeBenchmark name metric benchmark = do
  benchmarks <- traverse measure [0..100]
  pure $! bgroup name benchmarks
  where measure n = do
          input <- generate (resize n arbitrary)
          let measurement = metric input
          pure $! bench (show measurement) (benchmark input)


-- Instances

deriving instance (NFData a, NFData b) => NFData (These a b)
deriving instance NFData a => NFData (Join These a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = oneof [ This <$> arbitrary
                    , That <$> arbitrary
                    , These <$> arbitrary <*> arbitrary ]
  shrink = these (fmap This . shrink) (fmap That . shrink) (\ a b -> (This <$> shrink a) ++ (That <$> shrink b) ++ (These <$> shrink a <*> shrink b))

instance Arbitrary a => Arbitrary (Join These a) where
  arbitrary = Join <$> arbitrary
  shrink (Join a) = Join <$> shrink a
