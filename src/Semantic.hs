{-# LANGUAGE GADTs #-}
module Semantic
( parseAndRenderBlob
, parseDiffAndRenderBlobPair
, diffAndRenderTermPair
) where

import Data.Functor.Both as Both
import Data.Record
import Diff
import Info
import Interpreter
import qualified Language
import Patch
import Parser
import Prologue
import Renderer
import Semantic.Task as Task
import Source
import Term

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

-- | A task to parse a 'SourceBlob' and render the resulting 'Term'.
parseAndRenderBlob :: TermRenderer output -> SourceBlob -> Task output
parseAndRenderBlob renderer blob@SourceBlob{..} = case renderer of
  JSONTermRenderer -> case blobLanguage of
    Just Language.Python -> parse pythonParser source >>= render (renderJSONTerm blob)
    language -> parse (parserForLanguage language) source >>= decorate identifierAlgebra >>= render (renderJSONTerm blob)
  SExpressionTermRenderer -> case blobLanguage of
    Just Language.Python -> parse pythonParser source >>= render renderSExpressionTerm . fmap (Info.Other "Term" :.)
    language -> parse (parserForLanguage language) source >>= render renderSExpressionTerm
  IdentityTermRenderer -> case blobLanguage of
    Just Language.Python -> pure Nothing
    language -> Just <$> parse (parserForLanguage language) source


-- | A task to parse a pair of 'SourceBlob's, diff them, and render the 'Diff'.
parseDiffAndRenderBlobPair :: DiffRenderer output -> Both SourceBlob -> Task (Maybe output)
parseDiffAndRenderBlobPair renderer blobs = case renderer of
  ToCDiffRenderer -> do
    terms <- distributeFor blobs $ \ blob -> do
      term <- parseSource blob
      decorate (declarationAlgebra (source blob)) term
    diffAndRenderTermPair blobs (runBothWith diffTerms) renderToC terms
  JSONDiffRenderer -> do
    terms <- distributeFor blobs (decorate identifierAlgebra <=< parseSource)
    diffAndRenderTermPair blobs (runBothWith diffTerms) renderJSONDiff terms
  PatchDiffRenderer -> distributeFor blobs parseSource >>= diffAndRenderTermPair blobs (runBothWith diffTerms) renderPatch
  SExpressionDiffRenderer -> case effectiveLanguage of
    Just Language.Python -> distributeFor blobs parseSource >>= diffAndRenderTermPair blobs (runBothWith replacing) (const renderSExpressionDiff)
    _ -> distributeFor blobs parseSource >>= diffAndRenderTermPair blobs (runBothWith diffTerms) (const renderSExpressionDiff)
  IdentityDiffRenderer -> do
    terms <- distributeFor blobs $ \ blob -> do
      term <- parseSource blob
      decorate (declarationAlgebra (source blob)) term
    diffAndRenderTermPair blobs (runBothWith diffTerms) (const identity) terms
  where effectiveLanguage = runBothWith (<|>) (blobLanguage <$> blobs)
        parseSource = parse (parserForLanguage effectiveLanguage) . source

-- | A task to diff a pair of 'Term's and render the 'Diff', producing insertion/deletion 'Patch'es for non-existent 'SourceBlob's.
diffAndRenderTermPair :: Functor f => Both SourceBlob -> Differ f a -> (Both SourceBlob -> Diff f a -> output) -> Both (Term f a) -> Task (Maybe output)
diffAndRenderTermPair blobs differ renderer terms = case runJoin (nonExistentBlob <$> blobs) of
  (True, True) -> pure Nothing
  (_, True) -> Just <$> render (renderer blobs) (deleting (Both.fst terms))
  (True, _) -> Just <$> render (renderer blobs) (inserting (Both.snd terms))
  _ -> diff differ terms >>= fmap Just . render (renderer blobs)
