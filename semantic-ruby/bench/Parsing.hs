{-# LANGUAGE TypeApplications         #-}

module Parsing (benchmarks) where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Foldable
import           Gauge
import           System.Exit (die)
import           System.FilePath.Glob
import           Language.Ruby
import qualified Language.Ruby.AST as Rb
import           AST.Unmarshal

benchmarks :: Benchmark
benchmarks = bgroup "parsing" [ rubyBenchmarks ]

rubyBenchmarks :: Benchmark
rubyBenchmarks = bench "ruby" $ parseAllFiles dir "*.rb"
  where dir = "../semantic/tmp/ruby-examples/ruby_spec/command_line"

parseAllFiles :: FilePath -> String -> Benchmarkable
parseAllFiles dir glob = nfIO $ do
  files <- globDir1 (compile glob) dir
  when (null files) (die $ "No files found in " <> dir)
  for_ files $ \ file -> do
    -- print (Path.toString file)
    contents <- B.readFile file
    either die pure =<< parseByteString @Rb.Program @() tree_sitter_ruby contents
