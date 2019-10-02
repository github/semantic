{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, KindSignatures, LambdaCase, MonoLocalBinds, QuantifiedConstraints, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Api.Diffs
  ( parseDiffBuilder
  , DiffOutputFormat(..)
  , diffGraph

  , decoratingDiffWith
  , DiffEffects

  , legacySummarizeDiffParsers
  , LegacySummarizeDiff(..)
  , summarizeDiffParsers
  , SummarizeDiff(..)
  ) where

import           Analysis.ConstructorName (ConstructorName)
import           Analysis.Decorator (decoratorWithAlgebra)
import           Analysis.TOCSummary (Declaration, HasDeclaration, declarationAlgebra)
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Graph
import           Data.JSON.Fields
import           Data.Language
import           Data.Term
import qualified Data.Text as T
import qualified Data.Vector as V
import           Diffing.Algorithm (Diffable)
import           Diffing.Interpreter (HasDiffFor(..), DiffTerms(..))
import           Parsing.Parser
import           Prologue
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import           Rendering.TOC
import           Semantic.Api.Bridge
import           Semantic.Config
import           Semantic.Proto.SemanticPB hiding (Blob, BlobPair)
import           Semantic.Task as Task
import           Semantic.Telemetry as Stat
import           Serializing.Format hiding (JSON)
import qualified Serializing.Format as Format
import           Source.Loc

data DiffOutputFormat
  = DiffJSONTree
  | DiffJSONGraph
  | DiffSExpression
  | DiffShow
  | DiffDotGraph
  deriving (Eq, Show)

parseDiffBuilder :: (Traversable t, DiffEffects sig m) => DiffOutputFormat -> t BlobPair -> m Builder
parseDiffBuilder DiffJSONTree    = distributeFoldMap jsonDiff >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blob pairs.
parseDiffBuilder DiffJSONGraph   = diffGraph >=> serialize Format.JSON
parseDiffBuilder DiffSExpression = distributeFoldMap (diffWith sexprDiffParsers sexprDiff)
parseDiffBuilder DiffShow        = distributeFoldMap (diffWith showDiffParsers showDiff)
parseDiffBuilder DiffDotGraph    = distributeFoldMap (diffWith dotGraphDiffParsers dotGraphDiff)

jsonDiff :: DiffEffects sig m => BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff blobPair = diffWith jsonTreeDiffParsers (pure . jsonTreeDiff blobPair) blobPair `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

diffGraph :: (Traversable t, DiffEffects sig m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = DiffTreeGraphResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: DiffEffects sig m => BlobPair -> m DiffTreeFileGraph
    go blobPair = diffWith jsonGraphDiffParsers (pure . jsonGraphDiff blobPair) blobPair
      `catchError` \(SomeException e) ->
        pure (DiffTreeFileGraph path lang mempty mempty (V.fromList [ParseError (T.pack (show e))]))
      where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair

type DiffEffects sig m = (Member (Error SomeException) sig, Member (Reader Config) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m)


dotGraphDiffParsers :: [(Language, SomeParser DOTGraphDiff Loc)]
dotGraphDiffParsers = aLaCarteParsers

class DiffTerms term => DOTGraphDiff term where
  dotGraphDiff :: (Carrier sig m, Member (Reader Config) sig) => DiffFor term Loc Loc -> m Builder

instance (ConstructorName syntax, Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => DOTGraphDiff (Term syntax) where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph


jsonGraphDiffParsers :: [(Language, SomeParser JSONGraphDiff Loc)]
jsonGraphDiffParsers = aLaCarteParsers

class DiffTerms term => JSONGraphDiff term where
  jsonGraphDiff :: BlobPair -> DiffFor term Loc Loc -> DiffTreeFileGraph

instance (ConstructorName syntax, Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => JSONGraphDiff (Term syntax) where
  jsonGraphDiff blobPair diff
    = let graph = renderTreeGraph diff
          toEdge (Edge (a, b)) = DiffTreeEdge (diffVertexId a) (diffVertexId b)
      in DiffTreeFileGraph path lang (V.fromList (vertexList graph)) (V.fromList (fmap toEdge (edgeList graph))) mempty where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair


jsonTreeDiffParsers :: [(Language, SomeParser JSONTreeDiff Loc)]
jsonTreeDiffParsers = aLaCarteParsers

class DiffTerms term => JSONTreeDiff term where
  jsonTreeDiff :: BlobPair -> DiffFor term Loc Loc -> Rendering.JSON.JSON "diffs" SomeJSON

instance (Diffable syntax, Eq1 syntax, Hashable1 syntax, ToJSONFields1 syntax, Traversable syntax) => JSONTreeDiff (Term syntax) where
  jsonTreeDiff = renderJSONDiff


sexprDiffParsers :: [(Language, SomeParser SExprDiff Loc)]
sexprDiffParsers = aLaCarteParsers

class DiffTerms term => SExprDiff term where
  sexprDiff :: (Carrier sig m, Member (Reader Config) sig) => DiffFor term Loc Loc -> m Builder

instance (ConstructorName syntax, Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => SExprDiff (Term syntax) where
  sexprDiff = serialize (SExpression ByConstructorName)


showDiffParsers :: [(Language, SomeParser ShowDiff Loc)]
showDiffParsers = aLaCarteParsers

class DiffTerms term => ShowDiff term where
  showDiff :: (Carrier sig m, Member (Reader Config) sig) => DiffFor term Loc Loc -> m Builder

instance (Diffable syntax, Eq1 syntax, Hashable1 syntax, Show1 syntax, Traversable syntax) => ShowDiff (Term syntax) where
  showDiff = serialize Show


legacySummarizeDiffParsers :: [(Language, SomeParser LegacySummarizeDiff Loc)]
legacySummarizeDiffParsers = aLaCarteParsers

class DiffTerms term => LegacySummarizeDiff term where
  legacyDecorateTerm :: Blob -> term Loc -> term (Maybe Declaration)
  legacySummarizeDiff :: BlobPair -> DiffFor term (Maybe Declaration) (Maybe Declaration) -> Summaries

instance (Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Traversable syntax) => LegacySummarizeDiff (Term syntax) where
  legacyDecorateTerm = decoratorWithAlgebra . declarationAlgebra
  legacySummarizeDiff = renderToCDiff


summarizeDiffParsers :: [(Language, SomeParser SummarizeDiff Loc)]
summarizeDiffParsers = aLaCarteParsers

class DiffTerms term => SummarizeDiff term where
  decorateTerm :: Blob -> term Loc -> term (Maybe Declaration)
  summarizeDiff :: BlobPair -> DiffFor term (Maybe Declaration) (Maybe Declaration) -> TOCSummaryFile

instance (Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Traversable syntax) => SummarizeDiff (Term syntax) where
  decorateTerm = decoratorWithAlgebra . declarationAlgebra
  summarizeDiff blobPair diff = foldr go (TOCSummaryFile path lang mempty mempty) (diffTOC diff)
    where
      path = T.pack $ pathKeyForBlobPair blobPair
      lang = bridging # languageForBlobPair blobPair

      toChangeType = \case
        "added" -> Added
        "modified" -> Modified
        "removed" -> Removed
        _ -> None

      go :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
      go TOCSummary{..} TOCSummaryFile{..}
        = TOCSummaryFile path language (V.cons (TOCSummaryChange summaryCategoryName summaryTermName (converting #? summarySpan) (toChangeType summaryChangeType)) changes) errors
      go ErrorSummary{..} TOCSummaryFile{..}
        = TOCSummaryFile path language changes (V.cons (TOCSummaryError errorText (converting #? errorSpan)) errors)


diffWith
  :: (forall term . c term => DiffTerms term, DiffEffects sig m)
  => [(Language, SomeParser c Loc)]
  -> (forall term . c term => DiffFor term Loc Loc -> m output)
  -> BlobPair
  -> m output
diffWith parsers render blobPair = parsePairWith parsers (render <=< diffTerms blobPair) blobPair

decoratingDiffWith
  :: forall ann c output m sig
  .  (forall term . c term => DiffTerms term, DiffEffects sig m)
  => [(Language, SomeParser c Loc)]
  -> (forall term . c term => Blob -> term Loc -> term ann)
  -> (forall term . c term => DiffFor term ann ann -> m output)
  -> BlobPair
  -> m output
decoratingDiffWith parsers decorate render blobPair = parsePairWith parsers (render <=< diffTerms blobPair . Join . bimap (decorate blobL) (decorate blobR) . runJoin) blobPair where
  (blobL, blobR) = fromThese errorBlob errorBlob (runJoin blobPair)
  errorBlob = Prelude.error "evaluating blob on absent side"

diffTerms :: (DiffTerms term, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (term ann) -> m (DiffFor term ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  let diff = diffTermPair (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs
