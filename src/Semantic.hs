{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Bifunctor
import Data.Blob
import Data.ByteString (ByteString)
import Data.Functor.Both as Both
import Data.Output
import Data.Record
import Decorators
import Diff
import Info
import Interpreter
import qualified Language
import Parser
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

parseBlobs :: Output output => TermRenderer output -> [Blob] -> Task ByteString
parseBlobs renderer = fmap toOutput . distributeFoldMap (parseBlob renderer) . filter blobExists

-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> Blob -> Task output
parseBlob renderer blob@Blob{..} = case (renderer, blobLanguage) of
  (ToCTermRenderer, Just Language.Markdown) -> parse markdownParser blob >>= decorate (markupSectionAlgebra blob) >>= render (renderToCTerm blob)
  (ToCTermRenderer, Just Language.Python) -> parse pythonParser blob >>= decorate (declarationAlgebra blob) >>= render (renderToCTerm blob)
  (ToCTermRenderer, _) -> parse syntaxParser blob >>= decorate (syntaxDeclarationAlgebra blob) >>= render (renderToCTerm blob)
  (JSONTermRenderer, Just Language.Markdown) -> parse markdownParser blob >>= decorate constructorLabel >>= render (renderJSONTerm blob)
  (JSONTermRenderer, Just Language.Python) -> parse pythonParser blob >>= decorate constructorLabel >>= render (renderJSONTerm blob)
  (JSONTermRenderer, Just Language.JSON) -> parse jsonParser blob >>= decorate constructorLabel >>= render (renderJSONTerm blob)
  (JSONTermRenderer, _) -> parse syntaxParser blob >>= decorate identifierAlgebra >>= render (renderJSONTerm blob)
  (SExpressionTermRenderer, Just Language.Markdown) -> parse markdownParser blob >>= decorate constructorLabel >>= render renderSExpressionTerm . fmap keepConstructorLabel
  (SExpressionTermRenderer, Just Language.Python) -> parse pythonParser blob >>= decorate constructorLabel >>= render renderSExpressionTerm . fmap keepConstructorLabel
  (SExpressionTermRenderer, Just Language.JSON) -> parse jsonParser blob >>= decorate constructorLabel >>= render renderSExpressionTerm . fmap keepConstructorLabel
  (SExpressionTermRenderer, _) -> parse syntaxParser blob >>= render renderSExpressionTerm . fmap keepCategory
  (IdentityTermRenderer, Just Language.Markdown) -> pure Nothing
  (IdentityTermRenderer, Just Language.Python) -> pure Nothing
  (IdentityTermRenderer, Just Language.JSON) -> pure Nothing
  (IdentityTermRenderer, _) -> Just <$> parse syntaxParser blob
  where syntaxParser = parserForLanguage blobLanguage



diffBlobPairs :: Output output => DiffRenderer output -> [Both Blob] -> Task ByteString
diffBlobPairs renderer = fmap toOutput . distributeFoldMap (diffBlobPair renderer) . filter (any blobExists)

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> Both Blob -> Task output
diffBlobPair renderer blobs = case (renderer, effectiveLanguage) of
  (ToCDiffRenderer, Just Language.Markdown) -> run (\ blob -> parse markdownParser blob >>= decorate (markupSectionAlgebra blob)) diffTerms (renderToCDiff blobs)
  (ToCDiffRenderer, Just Language.Python) -> run (\ blob -> parse pythonParser blob >>= decorate (declarationAlgebra blob)) diffTerms (renderToCDiff blobs)
  (ToCDiffRenderer, _) -> run (\ blob -> parse syntaxParser blob >>= decorate (syntaxDeclarationAlgebra blob)) diffSyntaxTerms (renderToCDiff blobs)
  (JSONDiffRenderer, Just Language.Markdown) -> run (parse markdownParser) diffTerms (renderJSONDiff blobs)
  (JSONDiffRenderer, Just Language.Python) -> run (parse pythonParser) diffTerms (renderJSONDiff blobs)
  (JSONDiffRenderer, Just Language.JSON) -> run (parse jsonParser) diffTerms (renderJSONDiff blobs)
  (JSONDiffRenderer, _) -> run (decorate identifierAlgebra <=< parse syntaxParser) diffSyntaxTerms (renderJSONDiff blobs)
  (PatchDiffRenderer, Just Language.JSON) -> run (parse jsonParser) diffTerms (renderPatch blobs)
  (PatchDiffRenderer, Just Language.Markdown) -> run (parse markdownParser) diffTerms (renderPatch blobs)
  (PatchDiffRenderer, Just Language.Python) -> run (parse pythonParser) diffTerms (renderPatch blobs)
  (PatchDiffRenderer, _) -> run (parse syntaxParser) diffSyntaxTerms (renderPatch blobs)
  (SExpressionDiffRenderer, Just Language.JSON) -> run (decorate constructorLabel <=< parse jsonParser) diffTerms (renderSExpressionDiff . bimap keepConstructorLabel keepConstructorLabel)
  (SExpressionDiffRenderer, Just Language.Markdown) -> run (decorate constructorLabel <=< parse markdownParser) diffTerms (renderSExpressionDiff . bimap keepConstructorLabel keepConstructorLabel)
  (SExpressionDiffRenderer, Just Language.Python) -> run (decorate constructorLabel <=< parse pythonParser) diffTerms (renderSExpressionDiff . bimap keepConstructorLabel keepConstructorLabel)
  (SExpressionDiffRenderer, _) -> run (parse syntaxParser) diffSyntaxTerms (renderSExpressionDiff . bimap keepCategory keepCategory)
  (IdentityDiffRenderer, _) -> run (\ blob -> parse syntaxParser blob >>= decorate (syntaxDeclarationAlgebra blob)) diffSyntaxTerms Just
  where effectiveLanguage = runBothWith (<|>) (blobLanguage <$> blobs)
        syntaxParser = parserForLanguage effectiveLanguage

        run :: Functor syntax => (Blob -> Task (Term syntax ann)) -> (Term syntax ann -> Term syntax ann -> Diff syntax ann ann) -> (Diff syntax ann ann -> output) -> Task output
        run parse diff renderer = distributeFor blobs parse >>= runBothWith (diffTermPair blobs diff) >>= render renderer

-- | A task to diff a pair of 'Term's, producing insertion/deletion 'Patch'es for non-existent 'Blob's.
diffTermPair :: Functor syntax => Both Blob -> Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Task (Diff syntax ann1 ann2)
diffTermPair blobs differ t1 t2 = case runJoin (blobExists <$> blobs) of
  (True, False) -> pure (deleting t1)
  (False, True) -> pure (inserting t2)
  _ -> time "diff" logInfo $ diff differ t1 t2
  where
    logInfo = let (a, b) = runJoin blobs in
            [ ("before_path", blobPath a)
            , ("before_language", maybe "" show (blobLanguage a))
            , ("after_path", blobPath b)
            , ("after_language", maybe "" show (blobLanguage b)) ]


keepCategory :: HasField fields Category => Record fields -> Record '[Category]
keepCategory = (:. Nil) . category

keepConstructorLabel :: Record (ConstructorLabel ': fields) -> Record '[ConstructorLabel]
keepConstructorLabel = (:. Nil) . rhead
