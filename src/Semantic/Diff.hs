{-# LANGUAGE GADTs #-}
module Semantic.Diff where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Data.Blob
import Data.Diff
import Data.JSON.Fields
import Data.Record
import Data.Term
import Diffing.Algorithm (Diffable)
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob)
import Semantic.Stat as Stat
import Semantic.Task as Task
import Serializing.Format

diffBlobPairs :: (Members '[Distribute WrappedTask, Task, Telemetry, Exc SomeException, IO] effs, Monoid output) => DiffRenderer output -> [BlobPair] -> Eff effs output
diffBlobPairs renderer = distributeFoldMap (WrapTask . diffBlobPair renderer)

-- | A task to parse a pair of 'Blob's, diff them, and render the 'Diff'.
diffBlobPair :: Members '[Distribute WrappedTask, Task, Telemetry, Exc SomeException, IO] effs => DiffRenderer output -> BlobPair -> Eff effs output
diffBlobPair renderer blobs
  | Just (SomeParser parser) <- someParser @'[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, IdentifierName, Show1, ToJSONFields1, Traversable] <$> effectiveLanguage
  = case renderer of
    ToCDiffRenderer         -> run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))                     renderToCDiff
    JSONDiffRenderer        -> run (          parse parser      >=> decorate constructorLabel >=> decorate identifierLabel) renderJSONDiff
    SExpressionDiffRenderer -> run (          parse parser)                                                                 (const id)              >>= serialize (SExpression ByConstructorName)
    DOTDiffRenderer         -> run (          parse parser)                                                                 (const renderTreeGraph) >>= serialize (DOT (diffStyle (pathKeyForBlobPair blobs)))
  | otherwise = noLanguageForBlob effectivePath
  where effectivePath = pathForBlobPair blobs
        effectiveLanguage = languageForBlobPair blobs

        run :: (Diffable syntax, Eq1 syntax, GAlign syntax, Show1 syntax, Traversable syntax) => Members [Distribute WrappedTask, Task, Telemetry, IO] effs => (Blob -> TaskEff (Term syntax (Record fields))) -> (BlobPair -> Diff syntax (Record fields) (Record fields) -> output) -> Eff effs output
        run parse renderer = do
          terms <- distributeFor blobs (WrapTask . parse)
          time "diff" languageTag $ do
            diff <- diff (runJoin terms)
            writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
            render (renderer blobs) diff
          where
            languageTag = languageTagForBlobPair blobs
