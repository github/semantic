{-# LANGUAGE DeriveAnyClass, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Arguments
import Criterion.Main
import Data.Function
import Data.List (genericLength)
import Data.String
import Patch
import Prologue
import qualified Renderer as R
import SemanticDiff (fetchDiffs)
import qualified SemanticDiffPar
import SES
import System.Directory (makeAbsolute)

main :: IO ()
main = defaultMain
  [ bgroup "ses"
    [ bench "0,0" (nf (uncurry benchmarkSES) ([], []))
    , bench "1,1, =" (nf (uncurry benchmarkSES) ([lower], [lower]))
    , bench "1,1, ≠" (nf (uncurry benchmarkSES) ([lower], [upper]))
    , bench "10,10, =" (nf (uncurry benchmarkSES) (replicate 10 lower, replicate 10 lower))
    , bench "10,10, ≠" (nf (uncurry benchmarkSES) (replicate 10 lower, replicate 10 upper))
    , bench "100,100, =" (nf (uncurry benchmarkSES) (replicate 100 lower, replicate 100 lower))
    , bench "100,100, ≠" (nf (uncurry benchmarkSES) (replicate 100 lower, replicate 100 upper))
    ]
  , syncAsyncBenchmark
  ]
  where lower = ['a'..'z']
        upper = ['A'..'Z']

benchmarkSES :: [String] -> [String] -> [Either String (Patch String)]
benchmarkSES = ses compare cost
  where compare a b = if a == b then Just (Left a) else Nothing
        cost = either (const 0) (sum . fmap genericLength)

instance NFData a => NFData (Patch a)

syncAsyncBenchmark :: Benchmark
syncAsyncBenchmark =
  bgroup "async vs par" [
      bench "async" . whnfIO $ SemanticDiff.fetchDiffs =<< theArgs,
      bench "par" . whnfIO $ SemanticDiffPar.fetchDiffs =<< theArgs
    ]

theArgs :: IO Arguments
theArgs = do
  jqueryPath <- makeAbsolute "test/repos/jquery"
  pure $ args jqueryPath sha1 sha2 files R.Patch
  where
    sha1 = "70526981916945dc4093e116a3de61b1777d4718"
    sha2 = "e5ffcb0838c894e26f4ff32dfec162cf624d8d7d"
    files = [
            "src/manipulation/getAll.js",
            "src/manipulation/support.js",
            "src/manipulation/wrapMap.js",
            "src/offset.js",
            "test/unit/css.js",
            "test/unit/deferred.js",
            "test/unit/deprecated.js",
            "test/unit/effects.js",
            "test/unit/event.js",
            "test/unit/offset.js",
            "test/unit/wrap.js"
            ]
