{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies, TypeApplications #-}

module Main where

import           Control.Monad
import           Criterion.Main
import qualified Data.Language as Language
import           Data.Proxy
import           Parsing.Parser
import           Semantic.Config (defaultOptions)
import           Semantic.Task (withOptions)
import           Semantic.Util hiding (evalRubyProject, evalPythonProject, evaluateProject)

-- Duplicating this stuff from Util to shut off the logging
evalRubyProject       = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Ruby)       rubyParser
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser

evaluateProject proxy parser paths = withOptions defaultOptions $ \ config logger statter ->
  evaluateProject' (TaskConfig config logger statter) proxy parser paths

-- We use `fmap show` to ensure that all the parts of the result of evaluation are
-- evaluated themselves. While an NFData instance is the most morally correct way
-- to do this, I'm reluctant to add NFData instances to every single datatype in the
-- project—coercing the result into a string will suffice, though it throws off the
-- memory allocation results a bit.
pyEval :: FilePath -> Benchmarkable
pyEval p = nfIO . evalPythonProject $ ["bench/bench-fixtures/python/" <> p]

rbEval :: FilePath -> Benchmarkable
rbEval p = nfIO . evalRubyProject $ ["bench/bench-fixtures/ruby/" <> p]

pyCall :: FilePath -> Benchmarkable
pyCall p = nfIO $ callGraphProject pythonParser (Proxy @'Language.Python) defaultOptions ["bench/bench-fixtures/python/" <> p]

rbCall :: FilePath -> Benchmarkable
rbCall p = nfIO $ callGraphProject rubyParser (Proxy @'Language.Ruby) defaultOptions ["bench/bench-fixtures/ruby/" <> p]

main :: IO ()
main = defaultMain
  [ bgroup "python" [ bench "assignment" $ pyEval "simple-assignment.py"
                    , bench "function def" $ pyEval "function-definition.py"
                    , bench "if + function calls" $ pyEval "if-statement-functions.py"
                    , bench "call graph" $ pyCall "if-statement-functions.py"
                    ]
  , bgroup "ruby" [ bench "assignment" $ rbEval "simple-assignment.rb"
                  , bench "function def" $ rbEval "function-definition.rb"
                  , bench "if + function calls" $ rbEval "if-statement-functions.rb"
                  , bench "call graph" $ rbCall "if-statement-functions.rb"
                  ]
  ]
