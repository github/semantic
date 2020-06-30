module Main (main) where

import Gauge
import qualified Tagging

main :: IO ()
main = defaultMain
  [ Tagging.benchmarks
  ]
