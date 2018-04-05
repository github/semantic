{-# LANGUAGE GADTs #-}
module Semantic.Diff where

import Prologue hiding (MonadError(..))
import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Data.Blob
import Data.Diff
import Data.JSON.Fields
import Data.Output
import Data.Record
import Data.Term
import Diffing.Algorithm (Diffable)
import Diffing.Interpreter
import Parsing.Parser
import Rendering.Renderer
import Semantic.IO (NoLanguageForBlob(..))
import Semantic.Stat as Stat
import Semantic.Task as Task

diffBlobPairs :: (Members '[Distribute WrappedTask, Task, Telemetry, Exc SomeException, IO] effs, Output output) => DiffRenderer output -> [BlobPair] -> Eff effs ByteString
diffBlobPairs renderer blobs = toOutput' <$> distributeFoldMap (WrapTask . diffBlobPair renderer) blobs
  where toOutput' = case renderer of
          JSONDiffRenderer -> toOutput . renderJSONDiffs
          _ -> toOutput

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: Members '[Distribute WrappedTask, Task, Telemetry, Exc SomeException, IO] effs => DiffRenderer output -> BlobPair -> Eff effs output
diffBlobPair renderer blobs
  | Just (SomeParser parser) <- someParser (Proxy :: Proxy '[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, IdentifierName, Show1, ToJSONFields1, Traversable]) <$> effectiveLanguage
  = case renderer of
    ToCDiffRenderer         -> run (WrapTask . (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob)))                     diffTerms renderToCDiff
    JSONDiffRenderer        -> run (WrapTask . (          parse parser      >=> decorate constructorLabel >=> decorate identifierLabel)) diffTerms renderJSONDiff
    SExpressionDiffRenderer -> run (WrapTask . (          parse parser      >=> decorate constructorLabel . (Nil <$)))                   diffTerms (const renderSExpressionDiff)
    DOTDiffRenderer         -> run (WrapTask .            parse parser)                                                                  diffTerms renderDOTDiff
  | otherwise = throwError (SomeException (NoLanguageForBlob effectivePath))
  where effectivePath = pathForBlobPair blobs
        effectiveLanguage = languageForBlobPair blobs

        run :: (Foldable syntax, Functor syntax) => Members [Distribute WrappedTask, Task, Telemetry, IO] effs => (Blob -> WrappedTask (Term syntax ann)) -> (Term syntax ann -> Term syntax ann -> Diff syntax ann ann) -> (BlobPair -> Diff syntax ann ann -> output) -> Eff effs output
        run parse diff renderer = do
          terms <- distributeFor blobs parse
          time "diff" languageTag $ do
            diff <- diffTermPair diff (runJoin terms)
            writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
            render (renderer blobs) diff
          where
            languageTag = languageTagForBlobPair blobs

-- | A task to diff 'Term's, producing insertion/deletion 'Patch'es for non-existent 'Blob's.
diffTermPair :: (Functor syntax, Member Task effs) => Differ syntax ann1 ann2 -> These (Term syntax ann1) (Term syntax ann2) -> Eff effs (Diff syntax ann1 ann2)
diffTermPair _      (This  t1   ) = pure (deleting t1)
diffTermPair _      (That     t2) = pure (inserting t2)
diffTermPair differ (These t1 t2) = diff differ t1 t2
