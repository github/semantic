{-# LANGUAGE TypeApplications         #-}

module Parsing (benchmarks) where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Foldable
import           Gauge
import           System.Exit (die)
import           System.FilePath.Glob
import qualified System.Path as Path
import           TreeSitter.Ruby
import qualified TreeSitter.Ruby.AST as Rb
import           TreeSitter.Unmarshal

benchmarks :: Benchmark
benchmarks = bgroup "parsing" [ rubyBenchmarks ]

rubyBenchmarks :: Benchmark
rubyBenchmarks = bench "ruby" $ parseAllFiles dir "*.rb"
  where dir = Path.relDir "../semantic/tmp/ruby-examples/ruby_spec/command_line"

parseAllFiles :: Path.RelDir -> String -> Benchmarkable
parseAllFiles dir glob = nfIO $ do
  files <- globDir1 (compile glob) (Path.toString dir)
  let paths = Path.relFile <$> files
  when (null paths) (die $ "No files found in " <> (Path.toString dir))
  for_ paths $ \ file -> do
    -- print (Path.toString file)
    contents <- B.readFile (Path.toString file)
    either die pure =<< parseByteString @Rb.Program @() tree_sitter_ruby contents
