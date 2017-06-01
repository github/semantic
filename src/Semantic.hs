{-# LANGUAGE GADTs #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Algorithm hiding (diff)
import Data.Align.Generic (GAlign)
import Data.Functor.Both as Both
import Data.Functor.Classes (Eq1, Show1)
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
diffBlobPair renderer blobs = case (renderer, effectiveLanguage) of
  (ToCDiffRenderer, _) -> run (\ blob -> parseSource blob >>= decorate (declarationAlgebra (source blob))) (runBothWith diffTerms) (renderToC blobs)
  (JSONDiffRenderer, Just Language.Python) -> run parsePython diffLinearly (renderJSONDiff blobs)
  (JSONDiffRenderer, _) -> run (decorate identifierAlgebra <=< parseSource) (runBothWith diffTerms) (renderJSONDiff blobs)
  (PatchDiffRenderer, Just Language.Python) -> run parsePython diffLinearly (renderPatch blobs)
  (PatchDiffRenderer, _) -> run parseSource (runBothWith diffTerms) (renderPatch blobs)
  (SExpressionDiffRenderer, Just Language.Python) -> run (decorate (Literally . constructorLabel) <=< parsePython) diffLinearly (renderSExpressionDiff . mapAnnotations ((:. Nil) . rhead))
  (SExpressionDiffRenderer, _) -> run parseSource (runBothWith diffTerms) (renderSExpressionDiff . mapAnnotations ((:. Nil) . category))
  (IdentityDiffRenderer, _) -> run (\ blob -> parseSource blob >>= decorate (declarationAlgebra (source blob))) (runBothWith diffTerms) identity
  where effectiveLanguage = runBothWith (<|>) (blobLanguage <$> blobs)
        parseSource = parse (parserForLanguage effectiveLanguage) . source
        parsePython = parse pythonParser . source

        run :: Functor f => (SourceBlob -> Task (Term f a)) -> (Both (Term f a) -> Diff f a) -> (Diff f a -> output) -> Task (Maybe output)
        run parse diff renderer = distributeFor blobs parse >>= diffTermPair blobs diff >>= traverse (render renderer)

        diffLinearly :: (Eq1 f, GAlign f, Show1 f, Traversable f) => Both (Term f (Record fields)) -> Diff f (Record fields)
        diffLinearly = runBothWith (decoratingWith constructorLabel (diffTermsWith linearly comparableByGAlign))

-- | A task to diff a pair of 'Term's, producing insertion/deletion 'Patch'es for non-existent 'SourceBlob's and 'Nothing' if neither blob exists.
diffTermPair :: Functor f => Both SourceBlob -> Differ f a -> Both (Term f a) -> Task (Maybe (Diff f a))
diffTermPair blobs differ terms = case runJoin (nonExistentBlob <$> blobs) of
  (True, True) -> pure Nothing
  (_, True) -> pure (Just (deleting (Both.fst terms)))
  (True, _) -> pure (Just (inserting (Both.snd terms)))
  _ -> Just <$> diff differ terms

newtype Literally = Literally ByteString

instance Show Literally where
  showsPrec _ (Literally s) = showString (toS s)
