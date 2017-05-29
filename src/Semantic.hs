{-# LANGUAGE GADTs #-}
module Semantic
( diffBlobPairs
, diffBlobPair
, parseBlobs
, parseBlob
) where

import qualified Control.Concurrent.Async as Async
import Data.Functor.Both
import Data.Record
import Diff
import Info
import Interpreter
import Patch
import Parser
import Prologue
import Renderer
import Source
import Syntax
import Term

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


-- Internal

renderConcurrently :: (Monoid output, StringConv output ByteString) => (input -> IO output) -> [input] -> IO ByteString
renderConcurrently f diffs = do
  outputs <- Async.mapConcurrently f diffs
  pure $ toS (mconcat outputs)
