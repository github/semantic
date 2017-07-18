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
import Data.Blob
import Data.Functor.Both as Both
import Data.Functor.Classes (Eq1, Show1)
import Data.Record
import Data.Source
import qualified Data.Syntax.Declaration as Declaration
import Data.Union
import Decorators
import Diff
import Info
import Interpreter
import qualified Language
import Patch
import Parser
import Prologue
import Renderer
import Semantic.Task as Task
import Term

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

parseBlobs :: (Monoid output, StringConv output ByteString) => TermRenderer output -> [Blob] -> Task ByteString
parseBlobs renderer = fmap toS . distributeFoldMap (parseBlob renderer) . filter blobExists

-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> Blob -> Task output
parseBlob renderer blob@Blob{..} = case (renderer, blobLanguage) of
  (ToCTermRenderer, Just Language.Markdown) -> parse markdownParser blobSource >>= decorate (markupSectionAlgebra blobSource) >>= render (renderToCTerm blob)
  (ToCTermRenderer, Just Language.Python) -> parse pythonParser blobSource >>= decorate (declarationAlgebra blobSource) . hoistCofree (weaken :: Union fs a -> Union (Declaration.Method ': fs) a) >>= render (renderToCTerm blob)
  (ToCTermRenderer, _) -> parse syntaxParser blobSource >>= decorate (syntaxDeclarationAlgebra blobSource) >>= render (renderToCTerm blob)
  (JSONTermRenderer, Just Language.Markdown) -> parse markdownParser blobSource >>= decorate constructorLabel >>= render (renderJSONTerm blob)
  (JSONTermRenderer, Just Language.Python) -> parse pythonParser blobSource >>= decorate constructorLabel >>= render (renderJSONTerm blob)
  (JSONTermRenderer, _) -> parse syntaxParser blobSource >>= decorate identifierAlgebra >>= render (renderJSONTerm blob)
  (SExpressionTermRenderer, Just Language.Markdown) -> parse markdownParser blobSource >>= decorate constructorLabel >>= render renderSExpressionTerm . fmap keepConstructorLabel
  (SExpressionTermRenderer, Just Language.Python) -> parse pythonParser blobSource >>= decorate constructorLabel >>= render renderSExpressionTerm . fmap keepConstructorLabel
  (SExpressionTermRenderer, _) -> parse syntaxParser blobSource >>= render renderSExpressionTerm . fmap keepCategory
  (IdentityTermRenderer, Just Language.Markdown) -> pure Nothing
  (IdentityTermRenderer, Just Language.Python) -> pure Nothing
  (IdentityTermRenderer, _) -> Just <$> parse syntaxParser blobSource
  where syntaxParser = parserForLanguage blobLanguage



diffBlobPairs :: (Monoid output, StringConv output ByteString) => DiffRenderer output -> [Both Blob] -> Task ByteString
diffBlobPairs renderer = fmap toS . distributeFoldMap (diffBlobPair renderer) . filter (any blobExists)

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> Both Blob -> Task output
diffBlobPair renderer blobs = case (renderer, effectiveLanguage) of
  (ToCDiffRenderer, Just Language.Markdown) -> run (\ blobSource -> parse markdownParser blobSource >>= decorate (markupSectionAlgebra blobSource)) diffLinearly (renderToCDiff blobs)
  (ToCDiffRenderer, Just Language.Python) -> run (\ blobSource -> parse pythonParser blobSource >>= decorate (declarationAlgebra blobSource) . hoistCofree (weaken :: Union fs a -> Union (Declaration.Method ': fs) a)) diffLinearly (renderToCDiff blobs)
  (ToCDiffRenderer, _) -> run (\ blobSource -> parse syntaxParser blobSource >>= decorate (syntaxDeclarationAlgebra blobSource)) diffTerms (renderToCDiff blobs)
  (JSONDiffRenderer, Just Language.Markdown) -> run (parse markdownParser) diffLinearly (renderJSONDiff blobs)
  (JSONDiffRenderer, Just Language.Python) -> run (parse pythonParser) diffLinearly (renderJSONDiff blobs)
  (JSONDiffRenderer, _) -> run (decorate identifierAlgebra <=< parse syntaxParser) diffTerms (renderJSONDiff blobs)
  (PatchDiffRenderer, Just Language.Markdown) -> run (parse markdownParser) diffLinearly (renderPatch blobs)
  (PatchDiffRenderer, Just Language.Python) -> run (parse pythonParser) diffLinearly (renderPatch blobs)
  (PatchDiffRenderer, _) -> run (parse syntaxParser) diffTerms (renderPatch blobs)
  (SExpressionDiffRenderer, Just Language.Markdown) -> run (decorate constructorLabel <=< parse markdownParser) diffLinearly (renderSExpressionDiff . mapAnnotations keepConstructorLabel)
  (SExpressionDiffRenderer, Just Language.Python) -> run (decorate constructorLabel <=< parse pythonParser) diffLinearly (renderSExpressionDiff . mapAnnotations keepConstructorLabel)
  (SExpressionDiffRenderer, _) -> run (parse syntaxParser) diffTerms (renderSExpressionDiff . mapAnnotations keepCategory)
  (IdentityDiffRenderer, _) -> run (\ blobSource -> parse syntaxParser blobSource >>= decorate (syntaxDeclarationAlgebra blobSource)) diffTerms Just
  where effectiveLanguage = runBothWith (<|>) (blobLanguage <$> blobs)
        syntaxParser = parserForLanguage effectiveLanguage

        run :: Functor f => (Source -> Task (Term f a)) -> (Both (Term f a) -> Diff f a) -> (Diff f a -> output) -> Task output
        run parse diff renderer = distributeFor blobs (parse . blobSource) >>= diffTermPair blobs diff >>= render renderer

        diffLinearly :: (Eq1 f, GAlign f, Show1 f, Traversable f) => Both (Term f (Record fields)) -> Diff f (Record fields)
        diffLinearly = decoratingWith constructorNameAndConstantFields (diffTermsWith linearly comparableByConstructor)

-- | A task to diff a pair of 'Term's, producing insertion/deletion 'Patch'es for non-existent 'Blob's.
diffTermPair :: Functor f => Both Blob -> Differ f a -> Both (Term f a) -> Task (Diff f a)
diffTermPair blobs differ terms = case runJoin (blobExists <$> blobs) of
  (True, False) -> pure (deleting (Both.fst terms))
  (False, True) -> pure (inserting (Both.snd terms))
  _ -> diff differ terms


keepCategory :: HasField fields Category => Record fields -> Record '[Category]
keepCategory = (:. Nil) . category

keepConstructorLabel :: Record (ConstructorLabel ': fields) -> Record '[ConstructorLabel]
keepConstructorLabel = (:. Nil) . rhead
