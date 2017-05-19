{-# LANGUAGE GADTs #-}
module Semantic
( diffBlobPairs
, diffBlobPair
, parseBlobs
, parseBlob
, parserForLanguage
) where

import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async
import Data.Functor.Both
import Data.Record
import Diff
import Info
import Interpreter
import Language
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
diffBlobPairs :: (Monoid output, StringConv output ByteString, HasField fields Category, NFData (Record fields)) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> DiffRenderer fields output -> [Both SourceBlob] -> IO ByteString
diffBlobPairs decorator renderer blobs = do
  diffs <- Async.mapConcurrently go blobs
  let diffs' = diffs >>= \ (blobs, diff) -> (,) blobs <$> toList diff
  toS <$> renderConcurrently (resolveDiffRenderer renderer) (diffs' `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blobPair = do
      diff <- diffBlobPair decorator blobPair
      pure (blobPair, diff)

-- | Diff a pair of SourceBlobs.
diffBlobPair :: (HasField fields Category, NFData (Record fields)) => (Source -> Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record fields)) -> Both SourceBlob -> IO (Maybe (Diff (Syntax Text) (Record fields)))
diffBlobPair decorator blobs = do
  terms <- Async.mapConcurrently parseBlob blobs
  pure $ case (runJoin blobs, runJoin (decorator . source <$> blobs <*> terms)) of
    ((left, right), (a, b)) | nonExistentBlob left && nonExistentBlob right -> Nothing
                            | nonExistentBlob right -> Just . pure $ Delete a
                            | nonExistentBlob left -> Just . pure $ Insert b
                            | otherwise -> Just $ runDiff (both a b)
  where
    runDiff terms = runBothWith diffTerms (terms `using` parTraversable rdeepseq)

-- | Parse a list of SourceBlobs and use the specified renderer to produce ByteString output.
parseBlobs :: (Monoid output, StringConv output ByteString) => ParseTreeRenderer DefaultFields output -> [SourceBlob] -> IO ByteString
parseBlobs renderer blobs = do
  terms <- traverse go (filter (not . nonExistentBlob) blobs)
  toS <$> renderConcurrently (resolveParseTreeRenderer renderer) (terms `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blob = do
      term <- parseBlob blob
      pure (blob, term)

-- | Parse a SourceBlob.
parseBlob :: SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
parseBlob blob@SourceBlob{..} = runParser (parserForLanguage blobLanguage) blob

-- | Return a parser for a given langauge or the lineByLineParser parser.
parserForLanguage :: Maybe Language -> Parser (SyntaxTerm Text DefaultFields)
parserForLanguage Nothing = LineByLineParser
parserForLanguage (Just language) = case language of
  C -> CParser
  TypeScript -> TypeScriptParser
  Markdown -> MarkdownParser
  Ruby -> RubyParser
  Language.Go -> GoParser


-- Internal

renderConcurrently :: (Monoid output, StringConv output ByteString) => (a -> b -> output) -> [(a, b)] -> IO output
renderConcurrently f diffs = do
  outputs <- Async.mapConcurrently (pure . uncurry f) diffs
  pure $ mconcat (outputs `using` parTraversable rseq)
