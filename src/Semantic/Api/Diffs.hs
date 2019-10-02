{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, KindSignatures, LambdaCase, MonoLocalBinds, RankNTypes, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}
module Semantic.Api.Diffs
  ( parseDiffBuilder
  , DiffOutputFormat(..)
  , diffGraph

  , diffWith
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
import           Data.Kind (Constraint)
import           Data.Language
import           Data.Term
import qualified Data.Text as T
import qualified Data.Vector as V
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
parseDiffBuilder DiffSExpression = distributeFoldMap (diffWith @Loc sexprDiffParsers (const id) sexprDiff)
parseDiffBuilder DiffShow        = distributeFoldMap (diffWith @Loc showDiffParsers (const id) showDiff)
parseDiffBuilder DiffDotGraph    = distributeFoldMap (diffWith @Loc dotGraphDiffParsers (const id) dotGraphDiff)

jsonDiff :: DiffEffects sig m => BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff blobPair = diffWith jsonTreeDiffParsers (const id) (pure . jsonTreeDiff blobPair) blobPair `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

diffGraph :: (Traversable t, DiffEffects sig m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = DiffTreeGraphResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: DiffEffects sig m => BlobPair -> m DiffTreeFileGraph
    go blobPair = diffWith jsonGraphDiffParsers (const id) (pure . jsonGraphDiff blobPair) blobPair
      `catchError` \(SomeException e) ->
        pure (DiffTreeFileGraph path lang mempty mempty (V.fromList [ParseError (T.pack (show e))]))
      where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair

type DiffEffects sig m = (Member (Error SomeException) sig, Member (Reader Config) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m)


dotGraphDiffParsers :: [(Language, SomeParser (DiffTerms & DOTGraphDiff) Loc)]
dotGraphDiffParsers = aLaCarteParsers

class HasDiffFor term => DOTGraphDiff term where
  dotGraphDiff :: (Carrier sig m, Member (Reader Config) sig) => DiffFor term Loc Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => DOTGraphDiff (Term syntax) where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph


jsonGraphDiffParsers :: [(Language, SomeParser (DiffTerms & JSONGraphDiff) Loc)]
jsonGraphDiffParsers = aLaCarteParsers

class HasDiffFor term => JSONGraphDiff term where
  jsonGraphDiff :: BlobPair -> DiffFor term Loc Loc -> DiffTreeFileGraph

instance (Foldable syntax, Functor syntax, ConstructorName syntax) => JSONGraphDiff (Term syntax) where
  jsonGraphDiff blobPair diff
    = let graph = renderTreeGraph diff
          toEdge (Edge (a, b)) = DiffTreeEdge (diffVertexId a) (diffVertexId b)
      in DiffTreeFileGraph path lang (V.fromList (vertexList graph)) (V.fromList (fmap toEdge (edgeList graph))) mempty where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair


jsonTreeDiffParsers :: [(Language, SomeParser (DiffTerms & JSONTreeDiff) Loc)]
jsonTreeDiffParsers = aLaCarteParsers

class HasDiffFor term => JSONTreeDiff term where
  jsonTreeDiff :: BlobPair -> DiffFor term Loc Loc -> Rendering.JSON.JSON "diffs" SomeJSON

instance ToJSONFields1 syntax => JSONTreeDiff (Term syntax) where
  jsonTreeDiff = renderJSONDiff


sexprDiffParsers :: [(Language, SomeParser (DiffTerms & SExprDiff) Loc)]
sexprDiffParsers = aLaCarteParsers

class HasDiffFor term => SExprDiff term where
  sexprDiff :: (Carrier sig m, Member (Reader Config) sig) => DiffFor term Loc Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => SExprDiff (Term syntax) where
  sexprDiff = serialize (SExpression ByConstructorName)


showDiffParsers :: [(Language, SomeParser (DiffTerms & ShowDiff) Loc)]
showDiffParsers = aLaCarteParsers

class HasDiffFor term => ShowDiff term where
  showDiff :: (Carrier sig m, Member (Reader Config) sig) => DiffFor term Loc Loc -> m Builder

instance Show1 syntax => ShowDiff (Term syntax) where
  showDiff = serialize Show


legacySummarizeDiffParsers :: [(Language, SomeParser (DiffTerms & LegacySummarizeDiff) Loc)]
legacySummarizeDiffParsers = aLaCarteParsers

class HasDiffFor term => LegacySummarizeDiff term where
  legacyDecorateTerm :: Blob -> term Loc -> term (Maybe Declaration)
  legacySummarizeDiff :: BlobPair -> DiffFor term (Maybe Declaration) (Maybe Declaration) -> Summaries

instance (Foldable syntax, Functor syntax, HasDeclaration syntax) => LegacySummarizeDiff (Term syntax) where
  legacyDecorateTerm = decoratorWithAlgebra . declarationAlgebra
  legacySummarizeDiff = renderToCDiff


summarizeDiffParsers :: [(Language, SomeParser (DiffTerms & SummarizeDiff) Loc)]
summarizeDiffParsers = aLaCarteParsers

class HasDiffFor term => SummarizeDiff term where
  decorateTerm :: Blob -> term Loc -> term (Maybe Declaration)
  summarizeDiff :: BlobPair -> DiffFor term (Maybe Declaration) (Maybe Declaration) -> TOCSummaryFile

instance (Foldable syntax, Functor syntax, HasDeclaration syntax) => SummarizeDiff (Term syntax) where
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


class (c1 term, c2 term) => ((c1 :: (* -> *) -> Constraint) & (c2 :: (* -> *) -> Constraint)) (term :: * -> *)

infixl 9 &

instance (c1 term, c2 term) => (c1 & c2) term

diffWith
  :: forall ann c output m sig
  .  DiffEffects sig m
  => [(Language, SomeParser (DiffTerms & c) Loc)]
  -> (forall term . c term => Blob -> term Loc -> term ann)
  -> (forall term . c term => DiffFor term ann ann -> m output)
  -> BlobPair
  -> m output
diffWith parsers decorate render blobPair = parsePairWith parsers (render <=< diffTerms blobPair . Join . bimap (decorate blobL) (decorate blobR) . runJoin) blobPair where
  (blobL, blobR) = fromThese errorBlob errorBlob (runJoin blobPair)
  errorBlob = Prelude.error "evaluating blob on absent side"

diffTerms :: (DiffTerms term, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (term ann) -> m (DiffFor term ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  let diff = diffTermPair (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs
