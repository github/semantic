{-# LANGUAGE GADTs #-}
module Semantic
( diffBlobPairs
, diffBlobPair
, parseBlobs
, parseBlob
) where

import Control.Parallel.Strategies
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
diffBlobPairs :: (Monoid output, StringConv output ByteString, HasField fields Category, NFData (Record fields)) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Renderer (Both SourceBlob, Diff (Syntax Text) (Record fields)) output -> [Both SourceBlob] -> IO ByteString
diffBlobPairs decorator renderer blobs = do
  diffs <- Async.mapConcurrently go blobs
  let diffs' = diffs >>= \ (blobs, diff) -> (,) blobs <$> toList diff
  toS <$> renderConcurrently (pure . runRenderer renderer) (diffs' `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blobPair = do
      diff <- diffBlobPair decorator blobPair
      pure (blobPair, diff)

-- | Diff a pair of SourceBlobs.
diffBlobPair :: (HasField fields Category, NFData (Record fields)) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Both SourceBlob -> IO (Maybe (Diff (Syntax Text) (Record fields)))
diffBlobPair decorator blobs = do
  terms <- Async.mapConcurrently (parseBlob decorator) blobs
  pure $ case (runJoin blobs, runJoin terms) of
    ((left, right), (a, b)) | nonExistentBlob left && nonExistentBlob right -> Nothing
                            | nonExistentBlob right -> Just . pure $ Delete a
                            | nonExistentBlob left -> Just . pure $ Insert b
                            | otherwise -> Just $ runDiff (both a b)
  where
    runDiff terms = runBothWith diffTerms (terms `using` parTraversable rdeepseq)

-- | Parse a list of SourceBlobs and use the specified renderer to produce ByteString output.
parseBlobs :: (Monoid output, StringConv output ByteString, NFData (Record fields)) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Renderer (Identity SourceBlob, Term (Syntax Text) (Record fields)) output -> [SourceBlob] -> IO ByteString
parseBlobs decorator renderer blobs =
  toS <$> renderConcurrently (fmap (runRenderer renderer) . go) (filter (not . nonExistentBlob) blobs)
  where
    go blob = do
      term <- decorator (source blob) <$> runParser (parserForLanguage (blobLanguage blob)) (source blob)
      pure (Identity blob, term)

-- | Parse a SourceBlob.
parseBlob :: NFData (Record fields) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> SourceBlob -> IO (Term (Syntax Text) (Record fields))
parseBlob decorator SourceBlob{..} = decorator source <$> runParser (parserForLanguage blobLanguage) source


-- Internal

renderConcurrently :: (Monoid output, StringConv output ByteString) => (input -> IO output) -> [input] -> IO output
renderConcurrently f diffs = do
  outputs <- Async.mapConcurrently f diffs
  pure $ mconcat (outputs `using` parTraversable rseq)
