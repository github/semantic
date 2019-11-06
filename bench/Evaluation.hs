{-# LANGUAGE DataKinds, FlexibleContexts, PackageImports, PartialTypeSignatures, TypeApplications, TypeFamilies #-}

module Evaluation (benchmarks) where

import           Control.Carrier.Parse.Simple
import qualified Data.Duration as Duration
import           Data.Abstract.Evaluatable
import           Data.Blob
import           Data.Blob.IO (readBlobFromFile')
import           Data.Bifunctor
import           "semantic" Data.Graph (topologicalSort)
import qualified Data.Language as Language
import           Data.Project
import           Data.Proxy
import           Gauge.Main
import           Parsing.Parser
import           Semantic.Config (defaultOptions)
import           Semantic.Graph
import           Semantic.Task (TaskSession (..), runTask, withOptions)
import           Semantic.Util
import qualified System.Path as Path
import           System.Path ((</>))

-- Duplicating this stuff from Util to shut off the logging

callGraphProject' :: ( Language.SLanguage lang
                     , HasPrelude lang
                     )
                  => TaskSession
                  -> Proxy lang
                  -> Path.RelFile
                  -> IO (Either String ())
callGraphProject' session proxy path
  | Just (SomeParser parser) <- parserForLanguage analysisParsers lang = fmap (bimap show (const ())) . runTask session $ do
  blob <- readBlobFromFile' (fileForTypedPath path)
  package <- fmap snd <$> runParse (Duration.fromSeconds 10) (parsePackage parser (Project (Path.toString (Path.takeDirectory path)) [blob] lang []))
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  runCallGraph proxy False modules package
  | otherwise = error $ "Analysis not supported for: " <> show lang
  where lang = Language.reflect proxy

callGraphProject proxy paths = withOptions defaultOptions $ \ config logger statter ->
  callGraphProject' (TaskSession config "" False logger statter) proxy paths

evaluateProject proxy path
  | Just (SomeParser parser) <- parserForLanguage analysisParsers lang = withOptions defaultOptions $ \ config logger statter ->
  fmap (const ()) . justEvaluating =<< evaluateProject' (TaskSession config "" False logger statter) proxy parser [Path.toString path]
  | otherwise = error $ "Analysis not supported for: " <> show lang
  where lang = Language.reflect proxy

pyEval :: Path.RelFile -> Benchmarkable
pyEval p = nfIO $ evaluateProject  (Proxy @'Language.Python) (Path.relDir "bench/bench-fixtures/python" </> p)

rbEval :: Path.RelFile -> Benchmarkable
rbEval p = nfIO $ evaluateProject  (Proxy @'Language.Ruby)   (Path.relDir "bench/bench-fixtures/ruby" </> p)

pyCall :: Path.RelFile -> Benchmarkable
pyCall p = nfIO $ callGraphProject (Proxy @'Language.Python) (Path.relDir "bench/bench-fixtures/python/" </> p)

rbCall :: Path.RelFile -> Benchmarkable
rbCall p = nfIO $ callGraphProject (Proxy @'Language.Ruby)   (Path.relDir "bench/bench-fixtures/ruby" </> p)

benchmarks :: Benchmark
benchmarks = bgroup "evaluation"
  [ bgroup "python" [ bench "assignment" . pyEval $ Path.relFile "simple-assignment.py"
                    , bench "function def" . pyEval $ Path.relFile "function-definition.py"
                    , bench "if + function calls" . pyCall . Path.relFile $ "if-statement-functions.py"
                    , bench "call graph" $ pyCall . Path.relFile $ "if-statement-functions.py"
                    ]
  , bgroup "ruby" [ bench "assignment" . rbEval $ Path.relFile "simple-assignment.rb"
                  , bench "function def" . rbEval . Path.relFile $ "function-definition.rb"
                  , bench "if + function calls" . rbCall $ Path.relFile "if-statement-functions.rb"
                  , bench "call graph" $ rbCall $ Path.relFile "if-statement-functions.rb"
                  ]
  ]
