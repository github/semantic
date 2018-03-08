module Main where

import Criterion.Main
import Semantic.Util
import Data.Monoid

pyEval :: FilePath -> Benchmarkable
pyEval = whnfIO . evaluatePythonFile . ("bench/bench-fixtures/python/" <>)

rbEval :: FilePath -> Benchmarkable
rbEval = whnfIO . evaluateRubyFile . ("bench/bench-fixtures/ruby/" <>)

main :: IO ()
main = defaultMain
  [ bgroup "python" [ bench "assignment" $ pyEval "simple-assignment.py"
                  , bench "function def" $ pyEval "function-definition.py"
                  , bench "if + function calls" $ pyEval "if-statement-functions.py"
                  ]
  , bgroup "ruby" [ bench "assignment" $ rbEval "simple-assignment.rb"
                  , bench "function def" $ rbEval "function-definition.rb"
                  , bench "if + function calls" $ rbEval "if-statement-functions.rb"
                  ]
  ]
