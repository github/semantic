{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

module Tagging (benchmarks) where

import           Control.Carrier.Parse.Measured
import           Control.Carrier.Reader
import           Control.Concurrent.Async (forConcurrently)
import           Control.Exception (displayException, throwIO)
import           Control.Lens
import           Control.Monad
import           Data.Blob
import           Data.Foldable
import           Data.Language (LanguageMode (..), PerLanguageModes (..))
import           Data.List
import qualified Data.Text as Text
import           Data.Traversable
import           Gauge
import           System.FilePath.Glob
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Process as Process

import Data.Flag
import Proto.Semantic as P hiding (Blob, BlobPair)
import Proto.Semantic_Fields as P
import Semantic.Api.Symbols (parseSymbols)
import Semantic.Config as Config
import Semantic.Task
import Semantic.Task.Files

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
parseSymbolsFilePath languageModes path = readBlob (fileForTypedPath path) >>= runReader languageModes . parseSymbols . pure @[]

aLaCarteLanguageModes :: PerLanguageModes
aLaCarteLanguageModes = PerLanguageModes
  { pythonMode = ALaCarte
  , rubyMode = ALaCarte
  , goMode = ALaCarte
  , typescriptMode = ALaCarte
  , tsxMode = ALaCarte
  , javascriptMode = ALaCarte
  , jsxMode = ALaCarte
  }

preciseLanguageModes :: PerLanguageModes
preciseLanguageModes = PerLanguageModes
  { pythonMode = Precise
  , rubyMode = Precise
  , goMode = Precise
  , typescriptMode = Precise
  , tsxMode = Precise
  , javascriptMode = Precise
  , jsxMode = Precise
  }

testOptions :: Config.Options
testOptions = defaultOptions
  { optionsFailOnWarning = flag FailOnWarning True
  , optionsLogLevel = Nothing
  }
