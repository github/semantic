{-# LANGUAGE GADTs #-}
module Semantic where

import Control.Parallel.Strategies
import Data.Functor.Both
import Data.RandomWalkSimilarity
import Data.Record
import Diff
import Info
import Interpreter
import Parser.Language
import Prologue
import Renderer
import Source
import Syntax
import Patch
import Term


-- TODO
-- x1. This interface needs to deal with [Both (Maybe SourceBlob)] to handle new files and deleted files.
-- x2. Switch all of SemanticCmdLine over to using these interfaces. It should first run `Command` to get blobs
-- 2a. Remove extraneous stuff from Command (no longer needs to diff or parse).
-- 3. Consider using [Patch SourceBlob] ??? Bad idea? Good idea? Who knows!?
-- 4. Move `sourceBlobsFromSha` and `sourceBlobsFromPaths` over into Command (used in runParse code paths).
-- 5. Think about naming. blobs? blobPairs?

-- TODO: This should be the primary interface to the Semantic library.
-- Goals:
--   - No knowledge of filesystem or Git
--   - Built in concurrency where appropriate
--   - Easy to consume this interface for both a cmdline or web server app.

-- | Diff a list of blob pairs and use the specified renderer to produce ByteString output.
diffBlobs :: (Monoid output, StringConv output ByteString) => DiffRenderer DefaultFields output -> [Both SourceBlob] -> IO ByteString
diffBlobs renderer blobs = do
  diffs <- traverse go blobs
  let diffs' = diffs >>= \ (blobs, diff) -> (,) blobs <$> toList diff
  -- pure . toS $ runDiffRenderer renderer diffs'
  pure . toS $ runDiffRenderer renderer (diffs' `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blobPair = do
      diff <- diffBlobs' blobPair
      pure (blobPair, diff)

-- | Diff a pair of blobs.
diffBlobs' :: Both SourceBlob -> IO (Maybe (Diff (Syntax Text) (Record DefaultFields)))
diffBlobs' blobs = do
  terms <- traverse parseBlob' blobs
  case (runJoin blobs, runJoin terms) of
    ((left, right), (a, b)) | nonExistentBlob left && nonExistentBlob right -> pure Nothing
                            | nonExistentBlob right -> pure . pure . pure $ Delete a
                            | nonExistentBlob left -> pure . pure . pure $ Insert b
                            | otherwise -> pure . pure $ runDiff terms
  where
    -- runDiff terms = stripDiff (runBothWith diffTerms (fmap decorate terms))
    runDiff terms = stripDiff (runBothWith diffTerms (fmap decorate (terms `using` parTraversable rdeepseq)))
    decorate = defaultFeatureVectorDecorator getLabel
    getLabel :: HasField fields Category => TermF (Syntax Text) (Record fields) a -> (Category, Maybe Text)
    getLabel (h :< t) = (Info.category h, case t of
      Leaf s -> Just s
      _ -> Nothing)

-- | Parse a list of blobs and use the specified renderer to produce ByteString output.
parseBlobs :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer DefaultFields output -> [SourceBlob] -> IO ByteString
parseBlobs renderer blobs = do
  terms <- traverse go blobs
  -- pure . toS $ runParseTreeRenderer renderer terms
  pure . toS $ runParseTreeRenderer renderer (terms `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blob = do
      term <- parseBlob' blob
      pure (blob, term)

-- | Parse a SourceBlob.
parseBlob' :: SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
parseBlob' blob@SourceBlob{..} = parserForFilePath path blob
