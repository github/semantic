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
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import           Data.Graph
import           Data.JSON.Fields
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Term
import qualified Data.Text as T
import           Diffing.Algorithm (Diffable)
import           Parsing.Parser
import           Prologue
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON()
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import           Semantic.Api.Bridge
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
jsonDiff f blobPair = doDiff blobPair (const pure) f `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

renderJSONTree :: (Applicative m, ToJSONFields1 syntax) => BlobPair -> Diff syntax Loc Loc -> m (Rendering.JSON.JSON "diffs" SomeJSON)
renderJSONTree blobPair = pure . renderJSONDiff blobPair

diffGraph :: (Traversable t, DiffEffects sig m) => t BlobPair -> m DiffTreeGraphResponse
-- diffGraph blobs = DiffTreeGraphResponse . V.fromList . toList <$> distributeFor blobs go
diffGraph blobs = do
  graph <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList graph
  where
    go :: (DiffEffects sig m) => BlobPair -> m DiffTreeFileGraph
    go blobPair = doDiff blobPair (const pure) render
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

        render :: (Foldable syntax, Functor syntax, ConstructorName syntax, Applicative m) => BlobPair -> Diff syntax Loc Loc -> m DiffTreeFileGraph
        render _ diff =
          let graph = renderTreeGraph diff
              toEdge (Edge (a, b)) = defMessage & P.source .~ a^.diffVertexId & P.target .~ b^.diffVertexId
          in pure $ defMessage
               & P.path .~ path
               & P.language .~ lang
               & P.vertices .~ vertexList graph
               & P.edges .~ fmap toEdge (edgeList graph)
               & P.errors .~ mempty


sexpDiff :: (DiffEffects sig m) => BlobPair -> m Builder
sexpDiff blobPair = doDiff blobPair (const pure) (const (serialize (SExpression ByConstructorName)))

showDiff :: (DiffEffects sig m) => BlobPair -> m Builder
showDiff blobPair = doDiff blobPair (const pure) (const (serialize Show))

dotGraphDiff :: (DiffEffects sig m) => BlobPair -> m Builder
dotGraphDiff blobPair = doDiff blobPair (const pure) render
  where render _ = serialize (DOT (diffStyle "diffs")) . renderTreeGraph

type DiffEffects sig m = (Member (Error SomeException) sig, Member Telemetry sig, Member Distribute sig, Member Task sig, Carrier sig m, MonadIO m)

type CanDiff syntax = (ConstructorName syntax, Diffable syntax, Eq1 syntax, HasDeclaration syntax, Hashable1 syntax, Show1 syntax, ToJSONFields1 syntax, Traversable syntax)
type Decorate m a b = forall syntax . CanDiff syntax => Blob -> Term syntax a -> m (Term syntax b)

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
  => BlobPair -> Decorate m Loc ann -> (forall syntax . CanDiff syntax => BlobPair -> Diff syntax ann ann -> m output) -> m output
doDiff blobPair decorate render = do
  SomeTermPair terms <- doParse blobPair decorate
  diff <- diffTerms blobPair terms
  render blobPair diff

diffTerms :: (CanDiff syntax, Member Task sig, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (Term syntax ann) -> m (Diff syntax ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  diff <- diff (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs

doParse :: (Member (Error SomeException) sig, Member Distribute sig, Member Task sig, Carrier sig m)
  => BlobPair -> Decorate m Loc ann -> m (SomeTermPair TermPairConstraints ann)
doParse blobPair decorate = case languageForBlobPair blobPair of
  Go         -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse goParser blob >>= decorate blob)
  Haskell    -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse haskellParser blob >>= decorate blob)
  JavaScript -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse tsxParser blob >>= decorate blob)
  JSON       -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse jsonParser blob >>= decorate blob)
  JSX        -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse tsxParser blob >>= decorate blob)
  Markdown   -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse markdownParser blob >>= decorate blob)
  Python     -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse pythonParser blob >>= decorate blob)
  Ruby       -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse rubyParser blob >>= decorate blob)
  TypeScript -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse typescriptParser blob >>= decorate blob)
  TSX        -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse tsxParser blob >>= decorate blob)
  PHP        -> SomeTermPair <$> distributeFor blobPair (\ blob -> parse phpParser blob >>= decorate blob)
  _          -> noLanguageForBlob (pathForBlobPair blobPair)

data SomeTermPair typeclasses ann where
  SomeTermPair :: ApplyAll typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . ApplyAll typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms
