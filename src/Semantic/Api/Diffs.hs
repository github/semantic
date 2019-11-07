{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MonoLocalBinds, RankNTypes, UndecidableInstances #-}
module Semantic.Api.Diffs
  ( parseDiffBuilder
  , DiffOutputFormat(..)

  , diffTerms
  , diffGraph
  ) where

import           Analysis.ConstructorName (ConstructorName)
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import           Data.Edit
import           Data.Graph
import           Data.JSON.Fields (ToJSONFields1)
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Term (IsTerm(..))
import qualified Data.Text as T
import           Diffing.Interpreter (DiffTerms(..))
import           Parsing.Parser
import           Prologue
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON()
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import           Semantic.Api.Bridge
import           Semantic.Config
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

parseDiffBuilder :: (Traversable t, Member (Error SomeException) sig, Member (Reader Config) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m) => DiffOutputFormat -> t BlobPair -> m Builder
parseDiffBuilder DiffJSONTree    = distributeFoldMap jsonDiff >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blob pairs.
parseDiffBuilder DiffJSONGraph   = diffGraph >=> serialize Format.JSON
parseDiffBuilder DiffSExpression = distributeFoldMap (parsePairWith diffParsers sexprDiff)
parseDiffBuilder DiffShow        = distributeFoldMap (parsePairWith diffParsers showDiff)
parseDiffBuilder DiffDotGraph    = distributeFoldMap (parsePairWith diffParsers dotGraphDiff)

jsonDiff :: (Member (Error SomeException) sig, Member Telemetry sig, Member Parse sig, Carrier sig m, MonadIO m) => BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff blobPair = parsePairWith diffParsers jsonTreeDiff blobPair `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

diffGraph :: (Traversable t, Member (Error SomeException) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = do
  graph <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList graph
  where
    go :: (Member (Error SomeException) sig, Member Telemetry sig, Member Parse sig, Carrier sig m, MonadIO m) => BlobPair -> m DiffTreeFileGraph
    go blobPair = parsePairWith diffParsers jsonGraphDiff blobPair
      `catchError` \(SomeException e) ->
        pure $ defMessage
          & P.path .~ path
          & P.language .~ lang
          & P.vertices .~ mempty
          & P.edges .~ mempty
          & P.errors .~ [defMessage & P.error .~ T.pack (show e)]
      where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair


class DOTGraphDiff term where
  dotGraphDiff :: (Carrier sig m, Member (Reader Config) sig, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m Builder

instance (DiffTerms term, ConstructorName (Syntax term), Foldable (Syntax term), Functor (Syntax term)) => DOTGraphDiff term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms


class JSONGraphDiff term where
  jsonGraphDiff :: (Carrier sig m, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m DiffTreeFileGraph

instance (DiffTerms term, ConstructorName (Syntax term), Foldable (Syntax term), Functor (Syntax term)) => JSONGraphDiff term where
  jsonGraphDiff terms = do
    diff <- diffTerms terms
    let blobPair = bimap fst fst terms
        graph = renderTreeGraph diff
        toEdge (Edge (a, b)) = defMessage & P.source .~ a^.diffVertexId & P.target .~ b^.diffVertexId
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair
    pure $! defMessage
      & P.path     .~ path
      & P.language .~ lang
      & P.vertices .~ vertexList graph
      & P.edges    .~ fmap toEdge (edgeList graph)
      & P.errors   .~ mempty


class JSONTreeDiff term where
  jsonTreeDiff :: (Carrier sig m, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m (Rendering.JSON.JSON "diffs" SomeJSON)

instance (DiffTerms term, Foldable (Syntax term), ToJSONFields1 (Syntax term)) => JSONTreeDiff term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms


class SExprDiff term where
  sexprDiff :: (Carrier sig m, Member (Reader Config) sig, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m Builder

instance (DiffTerms term, ConstructorName (Syntax term), Foldable (Syntax term), Functor (Syntax term)) => SExprDiff term where
  sexprDiff = serialize (SExpression ByConstructorName) <=< diffTerms


class ShowDiff term where
  showDiff :: (Carrier sig m, Member (Reader Config) sig, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m Builder

instance (DiffTerms term, Foldable (Syntax term), Show1 (Syntax term)) => ShowDiff term where
  showDiff = serialize Show <=< diffTerms


diffTerms :: (DiffTerms term, Foldable (Syntax term), Member Telemetry sig, Carrier sig m, MonadIO m)
  => Edit (Blob, term ann) (Blob, term ann) -> m (Diff (Syntax term) ann ann)
diffTerms terms = time "diff" languageTag $ do
  let diff = diffTermPair (bimap snd snd terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs
        blobs = bimap fst fst terms

diffParsers :: Map Language (SomeParser Anything Loc)
diffParsers = aLaCarteParsers

class
  ( DiffTerms term
  , ConstructorName (Syntax term)
  , Foldable (Syntax term)
  , Functor (Syntax term)
  , Show1 (Syntax term)
  , ToJSONFields1 (Syntax term)
  ) => Anything term
instance
  ( DiffTerms term
  , ConstructorName (Syntax term)
  , Foldable (Syntax term)
  , Functor (Syntax term)
  , Show1 (Syntax term)
  , ToJSONFields1 (Syntax term)
  ) => Anything term
