module Main (main) where

import Gauge
import qualified Parsing

main :: IO ()
main = defaultMain [ Parsing.benchmarks ]
