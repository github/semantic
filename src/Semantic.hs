{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
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
parseBlobs renderer = fmap toS . distributeFoldMap (parseBlob renderer) . filter blobExists

-- | A task to parse a 'SourceBlob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> SourceBlob -> Task output
parseBlob renderer blob@SourceBlob{..} = case (renderer, blobLanguage) of
  (JSONTermRenderer, Just Language.Python) -> parse pythonParser source >>= render (renderJSONTerm blob)
  (JSONTermRenderer, _) -> parse syntaxParser source >>= decorate identifierAlgebra >>= render (renderJSONTerm blob)
  (SExpressionTermRenderer, Just Language.Python) -> parse pythonParser source >>= decorate (ConstructorLabel . constructorLabel) >>= render renderSExpressionTerm . fmap keepConstructorLabel
  (SExpressionTermRenderer, _) -> parse syntaxParser source >>= render renderSExpressionTerm . fmap keepCategory
  (IdentityTermRenderer, Just Language.Python) -> pure Nothing
  (IdentityTermRenderer, _) -> Just <$> parse syntaxParser source
  where syntaxParser = parserForLanguage blobLanguage


diffBlobPairs :: (Monoid output, StringConv output ByteString) => DiffRenderer output -> [Both SourceBlob] -> Task ByteString
diffBlobPairs renderer = fmap toS . distributeFoldMap (diffBlobPair renderer) . filter (any blobExists)

-- | A task to parse a pair of 'SourceBlob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> Both SourceBlob -> Task output
diffBlobPair renderer blobs = case (renderer, effectiveLanguage) of
  (ToCDiffRenderer, _) -> run (\ source -> parse syntaxParser source >>= decorate (syntaxDeclarationAlgebra source)) diffTerms (renderToC blobs)
  (JSONDiffRenderer, Just Language.Python) -> run (parse pythonParser) diffLinearly (renderJSONDiff blobs)
  (JSONDiffRenderer, _) -> run (decorate identifierAlgebra <=< parse syntaxParser) diffTerms (renderJSONDiff blobs)
  (PatchDiffRenderer, Just Language.Python) -> run (parse pythonParser) diffLinearly (renderPatch blobs)
  (PatchDiffRenderer, _) -> run (parse syntaxParser) diffTerms (renderPatch blobs)
  (SExpressionDiffRenderer, Just Language.Python) -> run (decorate (ConstructorLabel . constructorLabel) <=< parse pythonParser) diffLinearly (renderSExpressionDiff . mapAnnotations keepConstructorLabel)
  (SExpressionDiffRenderer, _) -> run (parse syntaxParser) diffTerms (renderSExpressionDiff . mapAnnotations keepCategory)
  (IdentityDiffRenderer, _) -> run (\ source -> parse syntaxParser source >>= decorate (syntaxDeclarationAlgebra source)) diffTerms Just
  where effectiveLanguage = runBothWith (<|>) (blobLanguage <$> blobs)
        syntaxParser = parserForLanguage effectiveLanguage

        run :: Functor f => (Source -> Task (Term f a)) -> (Both (Term f a) -> Diff f a) -> (Diff f a -> output) -> Task output
        run parse diff renderer = distributeFor blobs (parse . source) >>= diffTermPair blobs diff >>= render renderer

        diffLinearly :: (Eq1 f, GAlign f, Show1 f, Traversable f) => Both (Term f (Record fields)) -> Diff f (Record fields)
        diffLinearly = decoratingWith constructorLabel (diffTermsWith linearly comparableByConstructor)

-- | A task to diff a pair of 'Term's, producing insertion/deletion 'Patch'es for non-existent 'SourceBlob's.
diffTermPair :: Functor f => Both SourceBlob -> Differ f a -> Both (Term f a) -> Task (Diff f a)
diffTermPair blobs differ terms = case runJoin (blobExists <$> blobs) of
  (True, False) -> pure (deleting (Both.fst terms))
  (False, True) -> pure (inserting (Both.snd terms))
  _ -> diff differ terms


keepCategory :: HasField fields Category => Record fields -> Record '[Category]
keepCategory = (:. Nil) . category

keepConstructorLabel :: Record (ConstructorLabel ': fields) -> Record '[ConstructorLabel]
keepConstructorLabel = (:. Nil) . rhead


newtype ConstructorLabel = ConstructorLabel ByteString

instance Show ConstructorLabel where
  showsPrec _ (ConstructorLabel s) = showString (toS s)
