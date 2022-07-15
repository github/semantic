{-# LANGUAGE OverloadedStrings #-}
module AST.TestHelpers
  ( CorpusExample(..)
  , readCorpusFiles
  , readCorpusFiles'
  , parseCorpusFile
  , testCorpus
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Char8 (pack, unpack)
import Data.Either
import Data.Functor
import Prelude hiding (takeWhile)
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.FilePath.Glob
import Test.Tasty
import Test.Tasty.HUnit

testCorpus :: (ByteString -> IO (Either String (t a))) -> FilePath -> IO TestTree
testCorpus parse path = do
  xs <- parseCorpusFile path
  case xs of
    Left e   -> print ("Failed to parse corpus: " <> show path <> " " <> "Error: " <> show e) *> exitFailure
    Right xs -> testGroup path <$> traverse corpusTestCase xs
  where
    corpusTestCase (CorpusExample name code) = testCase name . either (errMsg code) pass <$> parse code
    pass = const (pure ())
    errMsg code e = assertFailure (e <> "\n``` \n" <> unpack code <> "```")

-- Depending on whether these tests are invoked via cabal run or cabal test,
-- we might be in a project subdirectory or not, so let's make sure we're
-- in project subdirectories as needed.
findCorpus :: FilePath -> IO FilePath
findCorpus p = do
  cwd <- getCurrentDirectory
  if takeFileName cwd == "haskell-tree-sitter"
     then pure p
     else pure (".." </> p)

-- The path is expected to be relative to the language project.
readCorpusFiles :: FilePath ->  IO [FilePath]
readCorpusFiles parent = do
  dir <- findCorpus parent
  globDir1 (compile "**/*.txt") dir

readCorpusFiles' :: FilePath ->  IO [FilePath]
readCorpusFiles' = globDir1 (compile "**/*.txt")

data CorpusExample = CorpusExample { name :: String, code :: ByteString }
  deriving (Eq, Show)

parseCorpusFile :: FilePath -> IO (Either String [CorpusExample])
parseCorpusFile path = do
  c <- Data.ByteString.readFile path
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
  where outputSepParser = (Attoparsec.take 3) *> (Attoparsec.char '-') *> endOfLine

exampleNameParser :: Parser String
exampleNameParser = do
  _ <- skipWhile (== '=') *> skipSpace
  name <- takeWhile (/= '\n')
  _ <- skipSpace *> skipWhile (== '=') *> skipSpace
  pure (unpack name)
