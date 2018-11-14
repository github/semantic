{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables #-}
module Semantic.Diff
  ( runDiff
  , diffBlobTOCPairs
  ) where

import           Analysis.ConstructorName (ConstructorName)
import           Analysis.TOCSummary (HasDeclaration, declarationAlgebra)
import           Control.Effect
import           Control.Effect.Error
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.Diff
import           Data.Graph.DiffVertex
import           Data.JSON.Fields
import           Data.Location
import           Data.Term
import           Diffing.Algorithm (Diffable)
import           Parsing.Parser
import           Prologue
import           Rendering.Graph
import           Rendering.JSON (SomeJSON (..))
import qualified Rendering.JSON as JSON
import           Rendering.Renderer
import           Semantic.Task as Task
import           Semantic.Telemetry as Stat
import           Serializing.Format

-- | Using the specified renderer, diff a list of 'BlobPair's to produce a 'Builder' output.
runDiff :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Member Telemetry sig, MonadIO m, Carrier sig m) => DiffRenderer output -> [BlobPair] -> m Builder
runDiff ToCDiffRenderer         = withParsedBlobPairs' renderJSONSummaryError (decorate . declarationAlgebra) (render . renderToCDiff) >=> serialize JSON
runDiff JSONDiffRenderer        = withParsedBlobPairs' renderJSONDiffError (const pure) (render . renderJSONDiff) >=> serialize JSON
runDiff JSONGraphDiffRenderer   = withParsedBlobPairs' renderJSONDiffError (const pure) (render . renderAdjGraph) >=> serialize JSON
  where renderAdjGraph :: (Recursive t, ToTreeGraph DiffVertex (Base t)) => BlobPair -> t -> JSON.JSON "diffs" SomeJSON
        renderAdjGraph blob diff = renderJSONAdjDiff blob (renderTreeGraph diff)
runDiff SExpressionDiffRenderer = withParsedBlobPairs (const pure) (const (serialize (SExpression ByConstructorName)))
runDiff ShowDiffRenderer        = withParsedBlobPairs (const pure) (const (serialize Show))
runDiff DOTDiffRenderer         = withParsedBlobPairs (const pure) (const (render renderTreeGraph)) >=> serialize (DOT (diffStyle "diffs"))

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms

diffBlobTOCPairs :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Member Telemetry sig, MonadIO m, Carrier sig m) => [BlobPair] -> m ([TOCSummary], [TOCSummary])
diffBlobTOCPairs = withParsedBlobPairs (decorate . declarationAlgebra) (render . renderRPCToCDiff)

type CanDiff syntax = (ConstructorName syntax, Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax)
type Decorate m a b = forall syntax . CanDiff syntax => Blob -> Term syntax a -> m (Term syntax b)

withParsedBlobPairs' :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Member Telemetry sig, MonadIO m, Monoid output, Carrier sig m)
                    => (BlobPair -> String -> output)
                    -> Decorate m Location ann
                    -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax ann ann -> m output)
                    -> [BlobPair]
                    -> m output
withParsedBlobPairs' onError decorate render = distributeFoldMap (\ blobs -> (withParsedBlobPair decorate blobs >>= withSomeTermPair (diffTerms blobs >=> render blobs)) `catchError` \(SomeException e) -> pure (onError blobs (show e)))
  where diffTerms :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member Task sig, Member Telemetry sig, Carrier sig m, MonadIO m) => BlobPair -> Join These (Term syntax ann) -> m (Diff syntax ann ann)
        diffTerms blobs terms = time "diff" languageTag $ do
          diff <- diff (runJoin terms)
          diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
          where languageTag = languageTagForBlobPair blobs

withParsedBlobPairs :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Member Telemetry sig, MonadIO m, Monoid output, Carrier sig m)
                    => Decorate m Location ann
                    -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax ann ann -> m output)
                    -> [BlobPair]
                    -> m output
withParsedBlobPairs decorate render = distributeFoldMap (\ blobs -> withParsedBlobPair decorate blobs >>= withSomeTermPair (diffTerms blobs >=> render blobs))
  where diffTerms :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member Task sig, Member Telemetry sig, Carrier sig m, MonadIO m) => BlobPair -> Join These (Term syntax ann) -> m (Diff syntax ann ann)
        diffTerms blobs terms = time "diff" languageTag $ do
          diff <- diff (runJoin terms)
          diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
          where languageTag = languageTagForBlobPair blobs

withParsedBlobPair :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m)
                   => Decorate m Location ann
                   -> BlobPair
                   -> m (SomeTermPair '[ConstructorName, Diffable, Eq1, HasDeclaration, Hashable1, Show1, ToJSONFields1, Traversable] ann)
withParsedBlobPair decorate blobs
  | Just (SomeParser parser) <- someParser @'[ConstructorName, Diffable, Eq1, HasDeclaration, Hashable1, Show1, ToJSONFields1, Traversable] (languageForBlobPair blobs)
    = SomeTermPair <$> distributeFor blobs (\ blob -> parse parser blob >>= decorate blob)
  | otherwise = noLanguageForBlob (pathForBlobPair blobs)
