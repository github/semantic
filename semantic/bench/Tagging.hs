{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tagging
( benchmarks
, runTagging
, pythonBenchmarks
, goBenchmarks
, rubyBenchmarks
) where

import           Control.Carrier.Parse.Measured
import           Control.Exception (throwIO)
import           Control.Monad
import           Data.Foldable
import           Gauge
import           System.FilePath.Glob
import qualified System.Path as Path

import qualified Analysis.File as File
import           Data.Flag
import           Proto.Semantic as P hiding (Blob)
import           Semantic.Api.Symbols (parseSymbols)
import           Semantic.Config as Config
import           Semantic.Task
import           Semantic.Task.Files

benchmarks :: Benchmark
benchmarks = bgroup "tagging"
  [ bench "jquery" $ runTagging' (Path.relFile "semantic/test/fixtures/jquery-3.5.1.min.js")
  , bench "sinatra" $ runTagging' (Path.relFile "semantic/test/fixtures/base.rb")
  ]
  -- Feel free to turn these on or write other benchmarks
  -- [ pythonBenchmarks
  -- , goBenchmarks
  -- , rubyBenchmarks
  -- ]

runTagging' :: Path.RelFile -> Benchmarkable
runTagging' path = nfIO . withOptions testOptions $ \ config logger statter -> do
  let session = TaskSession config "-" False logger statter
  runTask session (runParse (parseSymbolsFilePath path)) >>= either throwIO pure

pythonBenchmarks :: Benchmark
pythonBenchmarks = bgroup "python"
  [ bench "precise" $ runTagging pyDir "*.py"
  ]
  where pyDir = Path.relDir "tmp/python-examples/keras/keras"

goBenchmarks :: Benchmark
goBenchmarks = bgroup "go"
  [ bench "precise" $ runTagging dir "*.go"
  ]
  where dir = Path.relDir "tmp/go-examples/go/src/database/sql"

rubyBenchmarks :: Benchmark
rubyBenchmarks = bgroup "ruby"
  [ bench "precise" $ runTagging dir "*.rb"
  ]
  where dir = Path.relDir "tmp/ruby-examples/ruby_spec/command_line"

runTagging :: Path.RelDir -> String -> Benchmarkable
runTagging dir glob = nfIO . withOptions testOptions $ \ config logger statter -> do
  let session = TaskSession config "-" False logger statter
  files <- globDir1 (compile glob) (Path.toString dir)
  when (null files) (fail ("No files in " <> Path.toString dir))
  let paths = Path.relFile <$> files
  for_ paths (runTask session . runParse . parseSymbolsFilePath >=> either throwIO pure)

parseSymbolsFilePath ::
  ( Has (Error SomeException) sig m
  , Has Parse sig m
  , Has Files sig m
  )
  => Path.RelFile
  -> m ParseTreeSymbolResponse
parseSymbolsFilePath path = readBlob (File.fromPath path) >>= parseSymbols . pure @[]

testOptions :: Config.Options
testOptions = defaultOptions
  { optionsFailOnWarning = flag FailOnWarning True
  , optionsLogLevel = Nothing
  }
