{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveDataTypeable, GADTs, TypeOperators #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.Error.Class
import Data.Align.Generic
import Data.Bifoldable
import Data.Bifunctor.Join
import Data.Blob
import Data.ByteString (ByteString)
import Data.Diff
import Data.Functor.Classes
import Data.JSON.Fields
import Data.Output
import Data.Record
import Data.Term
import Data.Typeable
import Diffing.Algorithm (Diffable)
import Diffing.Interpreter
import Parsing.Parser
import Rendering.Renderer
import Semantic.Stat as Stat
import Semantic.Task as Task

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).

parseBlobs :: Output output => TermRenderer output -> [Blob] -> Task ByteString
parseBlobs renderer blobs = toOutput' <$> distributeFoldMap (parseBlob renderer) blobs
  where toOutput' = case renderer of
          JSONTermRenderer -> toOutput . renderJSONTerms
          _ -> toOutput

-- | A task to parse a 'Blob' and render the resulting 'Term'.
parseBlob :: TermRenderer output -> Blob -> Task output
parseBlob renderer blob@Blob{..}
  | Just (SomeParser parser) <- someParser (Proxy :: Proxy '[ConstructorName, HasDeclaration, IdentifierName, Foldable, Functor, ToJSONFields1]) <$> blobLanguage
  = parse parser blob >>= case renderer of
    ToCTermRenderer         -> decorate (declarationAlgebra blob)                     >=> render (renderToCTerm  blob)
    JSONTermRenderer        -> decorate constructorLabel >=> decorate identifierLabel >=> render (renderJSONTerm blob)
    SExpressionTermRenderer -> decorate constructorLabel . (Nil <$)                   >=> render renderSExpressionTerm
    TagsTermRenderer fields -> decorate (declarationAlgebra blob)                     >=> render (renderToTags fields blob)
    DOTTermRenderer         ->                                                            render (renderDOTTerm blob)
  | otherwise = throwError (SomeException (NoLanguageForBlob blobPath))

newtype NoLanguageForBlob = NoLanguageForBlob FilePath
  deriving (Eq, Exception, Ord, Show, Typeable)


diffBlobPairs :: Output output => DiffRenderer output -> [BlobPair] -> Task ByteString
diffBlobPairs renderer blobs = toOutput' <$> distributeFoldMap (diffBlobPair renderer) blobs
  where toOutput' = case renderer of
          JSONDiffRenderer -> toOutput . renderJSONDiffs
          _ -> toOutput

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: DiffRenderer output -> BlobPair -> Task output
diffBlobPair renderer blobs
  | Just (SomeParser parser) <- someParser (Proxy :: Proxy '[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, IdentifierName, Show1, ToJSONFields1, Traversable]) <$> effectiveLanguage
  = case renderer of
    ToCDiffRenderer         -> run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))                     diffTerms renderToCDiff
    JSONDiffRenderer        -> run (          parse parser      >=> decorate constructorLabel >=> decorate identifierLabel) diffTerms renderJSONDiff
    SExpressionDiffRenderer -> run (          parse parser      >=> decorate constructorLabel . (Nil <$))                   diffTerms (const renderSExpressionDiff)
    DOTDiffRenderer         -> run (          parse parser)                                                                 diffTerms renderDOTDiff
  | otherwise = throwError (SomeException (NoLanguageForBlob effectivePath))
  where effectivePath = pathForBlobPair blobs
        effectiveLanguage = languageForBlobPair blobs

        run :: (Foldable syntax, Functor syntax) => (Blob -> Task (Term syntax ann)) -> (Term syntax ann -> Term syntax ann -> Diff syntax ann ann) -> (BlobPair -> Diff syntax ann ann -> output) -> Task output
        run parse diff renderer = do
          terms <- bidistributeFor (runJoin blobs) parse parse
          time "diff" languageTag $ do
            diff <- diffTermPair diff terms
            writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
            render (renderer blobs) diff
          where
            languageTag = languageTagForBlobPair blobs

-- | A task to diff 'Term's, producing insertion/deletion 'Patch'es for non-existent 'Blob's.
diffTermPair :: Functor syntax => Differ syntax ann1 ann2 -> These (Term syntax ann1) (Term syntax ann2) -> Task (Diff syntax ann1 ann2)
diffTermPair _      (This  t1   ) = pure (deleting t1)
diffTermPair _      (That     t2) = pure (inserting t2)
diffTermPair differ (These t1 t2) = diff differ t1 t2
