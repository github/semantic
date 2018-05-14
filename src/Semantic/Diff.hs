{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Semantic.Diff where

import Analysis.ConstructorName (ConstructorName, constructorLabel)
import Analysis.IdentifierName (IdentifierName, identifierLabel)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Data.AST
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
  | Just (SomeParser parser) <- someParser @'[ConstructorName, Diffable, Eq1, GAlign, HasDeclaration, IdentifierName, Show1, ToJSONFields1, Traversable] <$> (languageForBlobPair blobs)
  = diffBlobPairWithParser renderer blobs parser
  | otherwise = noLanguageForBlob (pathForBlobPair blobs)

diffBlobPairWithParser :: forall syntax effs output . (ConstructorName syntax, Diffable syntax, Eq1 syntax, GAlign syntax, HasDeclaration syntax, IdentifierName syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax,  Members '[Distribute WrappedTask, Task, Telemetry, Exc SomeException, IO] effs) => DiffRenderer output -> BlobPair -> Parser (Term syntax (Record Location)) -> Eff effs output
diffBlobPairWithParser renderer blobs parser = case renderer of
  ToCDiffRenderer         -> parseBlobs (decorate . declarationAlgebra)                                  >>= diffTerms >>= render (renderToCDiff blobs)
  JSONDiffRenderer        -> parseBlobs (const (decorate constructorLabel >=> decorate identifierLabel)) >>= diffTerms >>= render (renderJSONDiff blobs)
  SExpressionDiffRenderer -> parseBlobs (const pure)                                                     >>= diffTerms                                   >>= serialize (SExpression ByConstructorName)
  DOTDiffRenderer         -> parseBlobs (const pure)                                                     >>= diffTerms >>= render renderTreeGraph        >>= serialize (DOT (diffStyle (pathKeyForBlobPair blobs)))
  where languageTag = languageTagForBlobPair blobs
        parseBlobs :: (Blob -> Term syntax (Record Location) -> TaskEff (Term syntax (Record fields))) -> Eff effs (Join These (Term syntax (Record fields)))
        parseBlobs decorate = distributeFor blobs (\ blob -> WrapTask (decorate blob =<< parse parser blob))
        diffTerms :: Join These (Term syntax (Record fields))
            -> Eff effs (Diff syntax (Record fields) (Record fields))
        diffTerms terms = time "diff" languageTag $ do
          diff <- diff (runJoin terms)
          diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
