{-# LANGUAGE AllowAmbiguousTypes, GeneralizedNewtypeDeriving, LambdaCase, MonoLocalBinds, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Semantic.Api.Diffs
  ( parseDiffBuilder
  , DiffOutputFormat(..)

  , diffTerms
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
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Term (IsTerm(..))
import qualified Data.Text as T
import           Diffing.Interpreter (DiffTerms(..))
import qualified Language.Go.Term as Go
import qualified Language.Markdown.Term as Markdown
import qualified Language.PHP.Term as PHP
import qualified Language.Python.Term as Python
import qualified Language.Ruby.Term as Ruby
import qualified Language.TSX.Term as TSX
import qualified Language.TypeScript.Term as TypeScript
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
parseDiffBuilder DiffSExpression = distributeFoldMap (parsePairWith sexprDiffParsers sexprDiff)
parseDiffBuilder DiffShow        = distributeFoldMap (parsePairWith showDiffParsers showDiff)
parseDiffBuilder DiffDotGraph    = distributeFoldMap (parsePairWith dotGraphDiffParsers dotGraphDiff)

jsonDiff :: (Member (Error SomeException) sig, Member Telemetry sig, Member Parse sig, Carrier sig m, MonadIO m) => BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff blobPair = parsePairWith jsonTreeDiffParsers jsonTreeDiff blobPair `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

diffGraph :: (Traversable t, Member (Error SomeException) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = do
  graph <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList graph
  where
    go :: (Member (Error SomeException) sig, Member Telemetry sig, Member Parse sig, Carrier sig m, MonadIO m) => BlobPair -> m DiffTreeFileGraph
    go blobPair = parsePairWith jsonGraphDiffParsers jsonGraphDiff blobPair
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


dotGraphDiffParsers :: Map Language (SomeParser DOTGraphDiff Loc)
dotGraphDiffParsers = aLaCarteParsers

class DOTGraphDiff term where
  dotGraphDiff :: (Carrier sig m, Member (Reader Config) sig, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m Builder

instance DOTGraphDiff Go.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms
instance DOTGraphDiff Markdown.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms
instance DOTGraphDiff PHP.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms
instance DOTGraphDiff Python.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms
instance DOTGraphDiff Ruby.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms
instance DOTGraphDiff TSX.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms
instance DOTGraphDiff TypeScript.Term where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph <=< diffTerms


jsonGraphDiffParsers :: Map Language (SomeParser JSONGraphDiff Loc)
jsonGraphDiffParsers = aLaCarteParsers

class JSONGraphDiff term where
  jsonGraphDiff :: (Carrier sig m, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m DiffTreeFileGraph

toGraph
  :: (Recursive diff, ToTreeGraph DiffTreeVertex (Base diff))
  => BlobPair
  -> diff
  -> DiffTreeFileGraph
toGraph blobPair diff =
  let graph = renderTreeGraph diff
      toEdge (Edge (a, b)) = defMessage & P.source .~ a^.diffVertexId & P.target .~ b^.diffVertexId
      path = T.pack $ pathForBlobPair blobPair
      lang = bridging # languageForBlobPair blobPair
  in defMessage
    & P.path     .~ path
    & P.language .~ lang
    & P.vertices .~ vertexList graph
    & P.edges    .~ fmap toEdge (edgeList graph)
    & P.errors   .~ mempty

instance JSONGraphDiff Go.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms
instance JSONGraphDiff Markdown.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms
instance JSONGraphDiff PHP.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms
instance JSONGraphDiff Python.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms
instance JSONGraphDiff Ruby.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms
instance JSONGraphDiff TSX.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms
instance JSONGraphDiff TypeScript.Term where
  jsonGraphDiff terms = toGraph (bimap fst fst terms) <$> diffTerms terms


jsonTreeDiffParsers :: Map Language (SomeParser JSONTreeDiff Loc)
jsonTreeDiffParsers = aLaCarteParsers

class JSONTreeDiff term where
  jsonTreeDiff :: (Carrier sig m, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m (Rendering.JSON.JSON "diffs" SomeJSON)

instance JSONTreeDiff Go.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms
instance JSONTreeDiff Markdown.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms
instance JSONTreeDiff PHP.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms
instance JSONTreeDiff Python.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms
instance JSONTreeDiff Ruby.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms
instance JSONTreeDiff TSX.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms
instance JSONTreeDiff TypeScript.Term where
  jsonTreeDiff terms = renderJSONDiff (bimap fst fst terms) <$> diffTerms terms


sexprDiffParsers :: Map Language (SomeParser SExprDiff Loc)
sexprDiffParsers = aLaCarteParsers

class SExprDiff term where
  sexprDiff :: (Carrier sig m, Member (Reader Config) sig, Member Telemetry sig, MonadIO m) => Edit (Blob, term Loc) (Blob, term Loc) -> m Builder

instance (DiffTerms term, ConstructorName (Syntax term), Foldable (Syntax term), Functor (Syntax term)) => SExprDiff term where
  sexprDiff = serialize (SExpression ByConstructorName) <=< diffTerms


showDiffParsers :: Map Language (SomeParser ShowDiff Loc)
showDiffParsers = aLaCarteParsers

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
