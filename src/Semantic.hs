{-# LANGUAGE GADTs #-}
module Semantic where

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
import Control.Parallel.Strategies
import Term


-- TODO: This should be the primary interface to the Semantic library.
-- Goals:
--   - No knowledge of filesystem or Git
--   - Built in concurrency where appropriate
--   - Easy to consume this interface for both a cmdline or web server app.

-- | Diff a list of blob pairs and use the specified renderer to produce ByteString output.
diffBlobs :: (Monoid output, StringConv output ByteString) => DiffRenderer DefaultFields output -> [Both SourceBlob] -> IO ByteString
diffBlobs renderer blobs = do
  diffs <- traverse go blobs
  pure . toS $ runDiffRenderer renderer (diffs `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blobPair = do
      diff <- diffBlobs' blobPair
      pure (blobPair, diff)

-- | Diff a pair of blobs.
diffBlobs' :: Both SourceBlob -> IO (Diff (Syntax Text) (Record DefaultFields))
diffBlobs' blobs = do
  terms <- traverse parseBlob' blobs
  pure $ stripDiff (runBothWith diffTerms (fmap decorate (terms `using` parTraversable rdeepseq)))
  where
    decorate = defaultFeatureVectorDecorator getLabel
    getLabel :: HasField fields Category => TermF (Syntax Text) (Record fields) a -> (Category, Maybe Text)
    getLabel (h :< t) = (Info.category h, case t of
      Leaf s -> Just s
      _ -> Nothing)

-- | Parse a list of blobs and use the specified renderer to produce ByteString output.
parseBlobs :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer DefaultFields output -> [SourceBlob] -> IO ByteString
parseBlobs renderer blobs = do
  terms <- traverse go blobs
  pure . toS $ runParseTreeRenderer renderer terms
  where
    go blob = do
      terms <- parseBlob' blob
      pure (blob, terms)

-- | Parse a SourceBlob.
parseBlob' :: SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
parseBlob' blob@SourceBlob{..} = parserForFilePath path blob
