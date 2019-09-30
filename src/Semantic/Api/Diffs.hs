{-# LANGUAGE GADTs, ConstraintKinds, TypeOperators, RankNTypes #-}
module Semantic.Api.Diffs
  ( parseDiffBuilder
  , DiffOutputFormat(..)
  , diffGraph

  , doDiff
  , DiffEffects

  , SomeTermPair(..)
  , withSomeTermPair
  ) where

import           Analysis.ConstructorName (ConstructorName)
import           Analysis.TOCSummary (HasDeclaration)
import           Control.Effect.Error
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import           Data.Graph
import           Data.JSON.Fields
import           Data.Language
import           Data.Term
import qualified Data.Text as T
import qualified Data.Vector as V
import           Diffing.Algorithm (Diffable)
import           Diffing.Interpreter (diffTermPair)
import           Parsing.Parser
import           Prologue
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
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
parseDiffBuilder DiffJSONTree    = distributeFoldMap (jsonDiff renderJSONTree) >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blob pairs.
parseDiffBuilder DiffJSONGraph   = diffGraph >=> serialize Format.JSON
parseDiffBuilder DiffSExpression = distributeFoldMap sexpDiff
parseDiffBuilder DiffShow        = distributeFoldMap showDiff
parseDiffBuilder DiffDotGraph    = distributeFoldMap dotGraphDiff

type RenderJSON m syntax = forall syntax . CanDiff syntax => BlobPair -> Diff syntax Loc Loc -> m (Rendering.JSON.JSON "diffs" SomeJSON)

jsonDiff :: (DiffEffects sig m) => RenderJSON m syntax -> BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff f blobPair = doDiff blobPair (const id) f `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

renderJSONTree :: (Applicative m, ToJSONFields1 syntax) => BlobPair -> Diff syntax Loc Loc -> m (Rendering.JSON.JSON "diffs" SomeJSON)
renderJSONTree blobPair = pure . renderJSONDiff blobPair

diffGraph :: (Traversable t, DiffEffects sig m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = DiffTreeGraphResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: (DiffEffects sig m) => BlobPair -> m DiffTreeFileGraph
    go blobPair = doDiff blobPair (const id) render
      `catchError` \(SomeException e) ->
        pure (DiffTreeFileGraph path lang mempty mempty (V.fromList [ParseError (T.pack (show e))]))
      where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair

        render :: (Foldable syntax, Functor syntax, ConstructorName syntax, Applicative m) => BlobPair -> Diff syntax Loc Loc -> m DiffTreeFileGraph
        render _ diff =
          let graph = renderTreeGraph diff
              toEdge (Edge (a, b)) = DiffTreeEdge (diffVertexId a) (diffVertexId b)
          in pure $ DiffTreeFileGraph path lang (V.fromList (vertexList graph)) (V.fromList (fmap toEdge (edgeList graph))) mempty


sexpDiff :: (DiffEffects sig m) => BlobPair -> m Builder
sexpDiff blobPair = doDiff blobPair (const id) (const (serialize (SExpression ByConstructorName)))

showDiff :: (DiffEffects sig m) => BlobPair -> m Builder
showDiff blobPair = doDiff blobPair (const id) (const (serialize Show))

dotGraphDiff :: (DiffEffects sig m) => BlobPair -> m Builder
dotGraphDiff blobPair = doDiff blobPair (const id) render
  where render _ = serialize (DOT (diffStyle "diffs")) . renderTreeGraph

type DiffEffects sig m = (Member (Error SomeException) sig, Member (Reader Config) sig, Member (Reader TaskSession) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m)

type CanDiff syntax = (ConstructorName syntax, Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax)
type Decorate a b = forall syntax . CanDiff syntax => Blob -> Term syntax a -> Term syntax b

type TermPairConstraints =
 '[ ConstructorName
  , Diffable
  , Eq1
  , HasDeclaration
  , Hashable1
  , Show1
  , Traversable
  , ToJSONFields1
  ]

doDiff :: (DiffEffects sig m)
  => BlobPair -> Decorate Loc ann -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax ann ann -> m output) -> m output
doDiff blobPair decorate render = do
  SomeTermPair terms <- doParse blobPair decorate
  diff <- diffTerms blobPair terms
  render blobPair diff

diffTerms :: (CanDiff syntax, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (Term syntax ann) -> m (Diff syntax ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  let diff = diffTermPair (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs

doParse :: (Member (Error SomeException) sig, Member Distribute sig, Member Parse sig, Carrier sig m)
  => BlobPair -> Decorate Loc ann -> m (SomeTermPair TermPairConstraints ann)
doParse blobPair decorate = case languageForBlobPair blobPair of
  Go         -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse goParser blob)
  Haskell    -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse haskellParser blob)
  JavaScript -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse tsxParser blob)
  JSON       -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse jsonParser blob)
  JSX        -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse tsxParser blob)
  Markdown   -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse markdownParser blob)
  Python     -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse pythonParser blob)
  Ruby       -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse rubyParser blob)
  TypeScript -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse typescriptParser blob)
  TSX        -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse tsxParser blob)
  PHP        -> SomeTermPair <$> distributeFor blobPair (\ blob -> decorate blob <$> parse phpParser blob)
  _          -> noLanguageForBlob (pathForBlobPair blobPair)

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms
