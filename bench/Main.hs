module Main (main) where

import Gauge
import qualified Evaluation

main :: IO ()
main = defaultMain
  [ Evaluation.benchmarks
  ]
