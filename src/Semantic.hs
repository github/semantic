{-# LANGUAGE GADTs #-}
module Semantic
( diffBlobs
, diffBlobs'
, parseBlobs
, parseBlob
, parserForLanguage
) where

import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async
import qualified Data.Text as T
import Data.Functor.Both
import Data.RandomWalkSimilarity
import Data.Record
import Data.String
import Data.List.Split
import Diff
import Info
import Interpreter
import Language
import Language.Markdown
import Parser
import Patch
import Prologue
import Renderer
import Source
import Syntax
import Term
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.JavaScript
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript
import TreeSitter


-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

-- | Diff a list of SourceBlob pairs to produce ByteString output using the specified renderer.
diffBlobs :: (Monoid output, StringConv output ByteString) => DiffRenderer DefaultFields output -> [Both SourceBlob] -> IO ByteString
diffBlobs renderer blobs = do
  diffs <- Async.mapConcurrently go blobs
  let diffs' = diffs >>= \ (blobs, diff) -> (,) blobs <$> toList diff
  toS <$> renderAsync (resolveDiffRenderer renderer) (diffs' `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blobPair = do
      diff <- diffBlobs' blobPair
      pure (blobPair, diff)

-- | Diff a pair of SourceBlobs.
diffBlobs' :: Both SourceBlob -> IO (Maybe (Diff (Syntax Text) (Record DefaultFields)))
diffBlobs' blobs = do
  terms <- Async.mapConcurrently parseBlob blobs
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
  toS <$> renderAsync (resolveParseTreeRenderer renderer) (terms `using` parTraversable (parTuple2 r0 rdeepseq))
  where
    go blob = do
      term <- parseBlob blob
      pure (blob, term)

-- | Parse a SourceBlob.
parseBlob :: SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
parseBlob blob@SourceBlob{..} = parserForFilePath path blob

-- | Return a parser for a given langauge or the lineByLineParser parser.
parserForLanguage :: Maybe Language -> Parser (Syntax Text) (Record DefaultFields)
parserForLanguage Nothing = lineByLineParser
parserForLanguage (Just language) = case language of
  C -> treeSitterParser C tree_sitter_c
  JavaScript -> treeSitterParser JavaScript tree_sitter_javascript
  TypeScript -> treeSitterParser TypeScript tree_sitter_typescript
  Markdown -> cmarkParser
  Ruby -> treeSitterParser Ruby tree_sitter_ruby
  Language.Go -> treeSitterParser Language.Go tree_sitter_go


-- | Internal

renderAsync :: (Monoid output, StringConv output ByteString) => (a -> b -> output) -> [(a, b)] -> IO output
renderAsync f diffs = do
  outputs <- Async.mapConcurrently (pure . uncurry f) diffs
  pure $ mconcat (outputs `using` parTraversable rseq)

-- | Return a parser based on the FilePath's extension (including the ".").
parserForFilePath :: FilePath -> Parser (Syntax Text) (Record DefaultFields)
parserForFilePath = parserForLanguage . languageForType . toS . takeExtension
  where
    -- System.FilePath defines a more robust version of this, but we don't want
    -- the core semantic library to have to know anything about filesystems and
    -- language detection is eventually a job for the calling program.
    -- See: https://github.com/haskell/filepath/blob/master/System/FilePath/Internal.hs
    takeExtension :: FilePath -> String
    takeExtension path =
      case rest of
        [] -> ""
        _ -> ext
      where
        (ext : rest) = reverse $ split (keepDelimsL $ whenElt (== '.')) path

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record DefaultFields)
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf charIndex line = (Range charIndex (charIndex + T.length line) :. Program :. rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (Source.toText line) ] , charIndex + Source.length line)
