{-# LANGUAGE DeriveAnyClass, FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Criterion.Main
import Data.Bifunctor.Join
import Data.String
import Data.These
import Prologue
import Test.QuickCheck

main :: IO ()
main = defaultMain
  []

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
