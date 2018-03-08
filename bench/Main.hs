module Main where

import Criterion.Main
import Semantic.Util
import Data.Monoid

pyEval :: FilePath -> Benchmarkable
pyEval = whnfIO . evaluatePythonFile . ("bench/bench-fixtures/python/" <>)



main :: IO ()
main = defaultMain [
  bgroup "python" [ bench "assignment" $ pyEval "simple-assignment.py"
                  , bench "function def" $ pyEval "function-definition.py"
                  , bench "if + function calls" $ pyEval "if-statement-functions.py"
                  ]
  ]
