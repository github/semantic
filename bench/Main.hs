{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module Main (main) where

import Gauge
import qualified Evaluation
import qualified Tagging

main :: IO ()
main = defaultMain
  [ Tagging.benchmarks
  -- , Evaluation.benchmarks
  ]
