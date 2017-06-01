{-# LANGUAGE GADTs #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Algorithm hiding (diff)
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
import Text.Show

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

parseBlobs :: (Monoid output, StringConv output ByteString) => TermRenderer output -> [SourceBlob] -> Task ByteString
parseBlobs renderer = fmap toS . distributeFoldMap (parseBlob renderer) . filter (not . nonExistentBlob)

-- | A task to parse a 'SourceBlob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> SourceBlob -> Task output
parseBlob renderer blob@SourceBlob{..} = case renderer of
  JSONTermRenderer -> case blobLanguage of
    Just Language.Python -> parse pythonParser source >>= render (renderJSONTerm blob)
    language -> parse (parserForLanguage language) source >>= decorate identifierAlgebra >>= render (renderJSONTerm blob)
  SExpressionTermRenderer -> case blobLanguage of
    Just Language.Python -> parse pythonParser source >>= decorate (Literally . constructorLabel) >>= render renderSExpressionTerm . fmap ((:. Nil) . rhead)
    language -> parse (parserForLanguage language) source >>= render renderSExpressionTerm . fmap ((:. Nil) . category)
  IdentityTermRenderer -> case blobLanguage of
    Just Language.Python -> pure Nothing
    language -> Just <$> parse (parserForLanguage language) source


diffBlobPairs :: (Monoid output, StringConv output ByteString) => DiffRenderer output -> [Both SourceBlob] -> Task ByteString
diffBlobPairs renderer = fmap toS . distributeFoldMap (fmap (fromMaybe mempty) . diffBlobPair renderer)

-- | A task to parse a pair of 'SourceBlob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> Both SourceBlob -> Task (Maybe output)
diffBlobPair renderer blobs = case renderer of
  ToCDiffRenderer -> do
    terms <- distributeFor blobs $ \ blob -> do
      term <- parseSource blob
      decorate (declarationAlgebra (source blob)) term
    diffTermPair blobs (runBothWith diffTerms) (renderToC blobs) terms
  JSONDiffRenderer -> case effectiveLanguage of
    Just Language.Python -> do
      terms <- distributeFor blobs (parse pythonParser . source)
      diffTermPair blobs (runBothWith (decoratingWith constructorLabel (diffTermsWith linearly comparableByGAlign))) (renderJSONDiff blobs) terms
    _ -> do
      terms <- distributeFor blobs (decorate identifierAlgebra <=< parseSource)
      diffTermPair blobs (runBothWith diffTerms) (renderJSONDiff blobs) terms
  PatchDiffRenderer -> distributeFor blobs parseSource >>= diffTermPair blobs (runBothWith diffTerms) (renderPatch blobs)
  SExpressionDiffRenderer -> case effectiveLanguage of
    Just Language.Python -> do
      terms <- distributeFor blobs (decorate (Literally . constructorLabel) <=< parse pythonParser . source)
      diffTermPair blobs (runBothWith (decoratingWith constructorLabel (diffTermsWith linearly comparableByGAlign))) (renderSExpressionDiff . mapAnnotations ((:. Nil) . rhead)) terms
    _ -> distributeFor blobs parseSource >>= diffTermPair blobs (runBothWith diffTerms) (renderSExpressionDiff . mapAnnotations ((:. Nil) . category))
  IdentityDiffRenderer -> do
    terms <- distributeFor blobs $ \ blob -> do
      term <- parseSource blob
      decorate (declarationAlgebra (source blob)) term
    diffTermPair blobs (runBothWith diffTerms) identity terms
  where effectiveLanguage = runBothWith (<|>) (blobLanguage <$> blobs)
        parseSource = parse (parserForLanguage effectiveLanguage) . source

-- | A task to diff a pair of 'Term's and render the 'Diff', producing insertion/deletion 'Patch'es for non-existent 'SourceBlob's.
diffTermPair :: Functor f => Both SourceBlob -> Differ f a -> (Diff f a -> output) -> Both (Term f a) -> Task (Maybe output)
diffTermPair blobs differ renderer terms = case runJoin (nonExistentBlob <$> blobs) of
  (True, True) -> pure Nothing
  (_, True) -> Just <$> render renderer (deleting (Both.fst terms))
  (True, _) -> Just <$> render renderer (inserting (Both.snd terms))
  _ -> diff differ terms >>= fmap Just . render renderer

newtype Literally = Literally ByteString

instance Show Literally where
  showsPrec _ (Literally s) = showString (toS s)
