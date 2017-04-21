{-# LANGUAGE GADTs #-}
module Semantic
( diffBlobs
, diffBlobs'
, parseBlobs
) where

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


-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Goals:
--   - No knowledge of filesystem or Git
--   - Built in concurrency where appropriate
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

-- | Diff a list of SourceBlob pairs and produce a ByteString using the specified renderer.
diffBlobs :: (Monoid output, StringConv output ByteString) => DiffRenderer DefaultFields output -> [Both SourceBlob] -> IO ByteString
diffBlobs renderer blobs = do
  diffs <- traverse go blobs
  let diffs' = diffs >>= \ (blobs, diff) -> (,) blobs <$> toList diff
  pure . toS $ runDiffRenderer renderer (diffs' `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blobPair = do
      diff <- diffBlobs' blobPair
      pure (blobPair, diff)

-- | Diff a pair of SourceBlobs.
diffBlobs' :: Both SourceBlob -> IO (Maybe (Diff (Syntax Text) (Record DefaultFields)))
diffBlobs' blobs = do
  terms <- traverse parseBlob blobs
  case (runJoin blobs, runJoin terms) of
    ((left, right), (a, b)) | nonExistentBlob left && nonExistentBlob right -> pure Nothing
                            | nonExistentBlob right -> pure . pure . pure $ Delete a
                            | nonExistentBlob left -> pure . pure . pure $ Insert b
                            | otherwise -> pure . pure $ runDiff terms
  where
    runDiff terms = stripDiff (runBothWith diffTerms (fmap decorate (terms `using` parTraversable rdeepseq)))
    decorate = defaultFeatureVectorDecorator getLabel
    getLabel :: HasField fields Category => TermF (Syntax Text) (Record fields) a -> (Category, Maybe Text)
    getLabel (h :< t) = (Info.category h, case t of
      Leaf s -> Just s
      _ -> Nothing)

-- | Parse a list of SourceBlobs and use the specified renderer to produce ByteString output.
parseBlobs :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer DefaultFields output -> [SourceBlob] -> IO ByteString
parseBlobs renderer blobs = do
  terms <- traverse go blobs
  pure . toS $ runParseTreeRenderer renderer (terms `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blob = do
      term <- parseBlob blob
      pure (blob, term)

-- | Parse a SourceBlob.
parseBlob :: SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
parseBlob blob@SourceBlob{..} = parserForFilePath path blob
