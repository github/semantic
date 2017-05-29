{-# LANGUAGE GADTs #-}
module Semantic
( diffBlobPairs
, diffBlobPair
, parseBlobs
, parseBlob
) where

import Data.Aeson (Value)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Functor.Both
import Data.Record
import Diff
import Info
import Interpreter
import qualified Language
import Patch
import Parser
import Prologue
import Renderer
import Source
import Syntax
import Term
import Text.Parser.TreeSitter.TypeScript

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

-- | Diff a list of SourceBlob pairs to produce ByteString output using the specified renderer.
diffBlobPairs :: (Monoid output, StringConv output ByteString, HasField fields Category) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) output -> [Both SourceBlob] -> IO ByteString
diffBlobPairs decorator renderer blobs = renderConcurrently parseDiffAndRender blobs
  where
    parseDiffAndRender blobPair = do
      diff <- diffBlobPair decorator blobPair
      pure $! case diff of
        Just a -> runRenderer renderer (blobPair, a)
        Nothing -> mempty

-- | Diff a pair of SourceBlobs.
diffBlobPair :: HasField fields Category => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Both SourceBlob -> IO (Maybe (Diff (Syntax Text) (Record fields)))
diffBlobPair decorator blobs = do
  terms <- Async.mapConcurrently (parseBlob decorator) blobs
  pure $ case (runJoin blobs, runJoin terms) of
    ((left, right), (a, b)) | nonExistentBlob left && nonExistentBlob right -> Nothing
                            | nonExistentBlob right -> Just $ deleting a
                            | nonExistentBlob left -> Just $ inserting b
                            | otherwise -> Just $ runDiff (both a b)
  where
    runDiff terms = runBothWith diffTerms terms

-- | Parse a list of SourceBlobs and use the specified renderer to produce ByteString output.
parseBlobs :: (Monoid output, StringConv output ByteString) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Renderer (Identity SourceBlob, Term (Syntax Text) (Record fields)) output -> [SourceBlob] -> IO ByteString
parseBlobs decorator renderer blobs = renderConcurrently parseAndRender (filter (not . nonExistentBlob) blobs)
  where
    parseAndRender blob = do
      term <- parseBlob decorator blob
      pure $! runRenderer renderer (Identity blob, term)

-- | Parse a SourceBlob.
parseBlob :: (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> SourceBlob -> IO (Term (Syntax Text) (Record fields))
parseBlob decorator SourceBlob{..} = decorator source <$> runParser (parserForLanguage blobLanguage) source

type Decorator input output = Source -> input -> output


data TaskF output where
  Parse :: Parser term -> Source -> TaskF term
  Decorate :: Decorator term term' -> Source -> term -> TaskF term'
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Monoid output => [Task output] -> TaskF output

type Task = Freer TaskF


data NamedDecorator = IdentifierDecorator

data NamedRenderer output where
  JSON :: NamedRenderer [Value]

parse :: Parser term -> Source -> Task term
parse parser source = Parse parser source `Then` return

decorate :: Decorator term term' -> Source -> term -> Task term'
decorate decorator source term = Decorate decorator source term `Then` return

render :: Renderer input output -> input -> Task output
render renderer input = Render renderer input `Then` return

distribute :: Monoid output => [Task output] -> Task output
distribute tasks = Distribute tasks `Then` return


parseAndRenderBlob :: NamedDecorator -> NamedRenderer output -> SourceBlob -> Task output
parseAndRenderBlob decorator renderer blob@SourceBlob{..} = case blobLanguage of
  Just Language.Python -> do
    term <- parse pythonParser source
    term' <- decorate (const identity) source term
    render (case renderer of
      JSON -> JSONRenderer) (Identity blob, term')
  Just Language.TypeScript -> do
    term <- parse (TreeSitterParser Language.TypeScript tree_sitter_typescript) source
    term' <- decorate (const identity) source term
    render (case renderer of
      JSON -> JSONRenderer) (Identity blob, term')


runTask :: Task a -> IO a
runTask = iterFreerA $ \ task yield -> case task of
  Parse parser source -> runParser parser source >>= yield
  Decorate decorator source term -> yield (decorator source term)
  Render renderer input -> yield (runRenderer renderer input)
  Distribute tasks -> do
    results <- Async.mapConcurrently runTask tasks
    yield (mconcat results)


-- Internal

renderConcurrently :: (Monoid output, StringConv output ByteString) => (input -> IO output) -> [input] -> IO ByteString
renderConcurrently f diffs = do
  outputs <- Async.mapConcurrently f diffs
  pure $ toS (mconcat outputs)
