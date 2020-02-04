{-# LANGUAGE OverloadedStrings #-}
module TreeSitter.Test.Helpers
  ( CorpusExample(..)
  , readCorpusFiles
  , parseCorpusFile
  , testCorpus
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString, readFile)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Either
import           Data.Functor
import           Prelude hiding (takeWhile)
import           System.Exit (exitFailure)
import           System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import           System.FilePath.Glob
import           Test.Tasty
import           Test.Tasty.HUnit

testCorpus :: (ByteString -> IO (Either String (t a))) -> Path.RelFile -> IO TestTree
testCorpus parse path = do
  xs <- parseCorpusFile path
  case xs of
    Left e -> print ("Failed to parse corpus: " <> show (Path.toString path) <> " " <> "Error: " <> show e) *> exitFailure
    Right xs -> testGroup (Path.toString path) <$> traverse corpusTestCase xs
  where
    corpusTestCase (CorpusExample name code) = testCase name . either (errMsg code) pass <$> parse code
    pass = const (pure ())
    errMsg code e = assertFailure (e <> "\n``` \n" <> unpack code <> "```")

-- Depending on whether these tests are invoked via cabal run or cabal test,
-- we might be in a project subdirectory or not, so let's make sure we're
-- in project subdirectories as needed.
findCorpus :: Path.RelDir -> IO Path.RelDir
findCorpus p = do
  cwd <- Path.getCurrentDirectory
  if Path.takeDirName cwd == Just (Path.relDir "haskell-tree-sitter")
     then pure p
     else pure (Path.relDir ".." </> p)

-- The path is expected to be relative to the language project.
readCorpusFiles :: Path.RelDir ->  IO [Path.RelFile]
readCorpusFiles parent = do
  dir <- findCorpus parent
  files <- globDir1 (compile "**/*.txt") (Path.toString dir)
  pure (Path.relPath <$> files)

data CorpusExample = CorpusExample { name :: String, code :: ByteString }
  deriving (Eq, Show)

parseCorpusFile :: Path.RelFile -> IO (Either String [CorpusExample])
parseCorpusFile path = do
  c <- Data.ByteString.readFile (Path.toString path)
  pure $ parseOnly corpusParser c

corpusParser :: Parser [CorpusExample]
corpusParser = do
  xs <- many' exampleParser
  void endOfInput
  pure xs

exampleParser :: Parser CorpusExample
exampleParser = do
  name <- exampleNameParser
  code <- manyTill anyChar outputSepParser
  _out <- manyTill anyChar (choice [endOfInput, char '=' $> ()])
  pure (CorpusExample name (pack code))
  where outputSepParser = choice [string "\n---\n", string "\r\n---\r\n"]

exampleNameParser :: Parser String
exampleNameParser = do
  _ <- skipWhile (== '=') *> skipSpace
  name <- takeWhile (/= '\n')
  _ <- skipSpace *> skipWhile (== '=') *> skipSpace
  pure (unpack name)
