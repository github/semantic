module Main where

import Criterion.Main
import Semantic.Util
import Data.Monoid
import Control.Monad

-- We use `fmap show` to ensure that all the parts of the result of evaluation are
-- evaluated themselves. While an NFData instance is the most morally correct way
-- to do this, I'm reluctant to add NFData instances to every single datatype in the
-- project—coercing the result into a string will suffice, though it throws off the
-- memory allocation results a bit.
pyEval :: FilePath -> Benchmarkable
pyEval = whnfIO . fmap show . evaluatePythonFile . ("bench/bench-fixtures/python/" <>)

rbEval :: FilePath -> Benchmarkable
rbEval = whnfIO . fmap show . evaluateRubyFile . ("bench/bench-fixtures/ruby/" <>)

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
