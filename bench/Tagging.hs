{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tagging (benchmarks) where

import           Control.Carrier.Parse.Measured
import           Control.Carrier.Reader
import           Control.Exception (throwIO)
import           Control.Monad
import           Data.Foldable
import           Data.Language (PerLanguageModes (..), aLaCarteLanguageModes, preciseLanguageModes)
import           Gauge
import           System.FilePath.Glob
import qualified System.Path as Path

import qualified Analysis.File as File
import           Data.Flag
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Semantic.Api.Symbols (parseSymbols)
import           Semantic.Config as Config
import           Semantic.Task
import           Semantic.Task.Files

benchmarks :: Benchmark
benchmarks = bgroup "tagging"
  [ pythonBenchmarks
  , goBenchmarks
  , rubyBenchmarks
  ]

pythonBenchmarks :: Benchmark
pythonBenchmarks = bgroup "python"
  [ bench "precise" $ runTagging preciseLanguageModes pyDir "*.py"
  , bench "a la carte" $ runTagging aLaCarteLanguageModes pyDir "*.py"
  ]
  where pyDir = Path.relDir "tmp/python-examples/keras/keras"

goBenchmarks :: Benchmark
goBenchmarks = bgroup "go"
  [ bench "precise" $ runTagging preciseLanguageModes dir "*.go"
  , bench "a la carte" $ runTagging aLaCarteLanguageModes dir "*.go"
  ]
  where dir = Path.relDir "tmp/go-examples/go/src/database/sql"

rubyBenchmarks :: Benchmark
rubyBenchmarks = bgroup "ruby"
  [ bench "precise" $ runTagging preciseLanguageModes dir "*.rb"
  , bench "a la carte" $ runTagging aLaCarteLanguageModes dir "*.rb"
  ]
  where dir = Path.relDir "tmp/ruby-examples/ruby_spec/command_line"

runTagging :: PerLanguageModes -> Path.RelDir -> String -> Benchmarkable
runTagging mode dir glob = nfIO . withOptions testOptions $ \ config logger statter -> do
  let session = TaskSession config "-" False logger statter
  files <- globDir1 (compile glob) (Path.toString dir)
  let paths = Path.relFile <$> files
  for_ paths (runTask session . runParse . parseSymbolsFilePath mode >=> either throwIO pure)

parseSymbolsFilePath ::
  ( Has (Error SomeException) sig m
  , Has Distribute sig m
  , Has Parse sig m
  , Has Files sig m
  )
  => PerLanguageModes
  -> Path.RelFile
  -> m ParseTreeSymbolResponse
parseSymbolsFilePath languageModes path = readBlob (File.fromPath path) >>= runReader languageModes . parseSymbols . pure @[]

testOptions :: Config.Options
testOptions = defaultOptions
  { optionsFailOnWarning = flag FailOnWarning True
  , optionsLogLevel = Nothing
  }
