{-# LANGUAGE GADTs, ConstraintKinds, FunctionalDependencies, LambdaCase, RankNTypes #-}
module Semantic.Api.Diffs
  ( parseDiffBuilder
  , DiffOutputFormat(..)
  , diffGraph

  , doDiff
  , DiffEffects

  , SomeTermPair(..)

  , LegacySummarizeDiff(..)
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
import           Data.Diff
import           Data.Graph
import           Data.JSON.Fields
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Term
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
import           Rendering.TOC
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

parseDiffBuilder :: (Traversable t, DiffEffects sig m) => DiffOutputFormat -> t BlobPair -> m Builder
parseDiffBuilder DiffJSONTree    = distributeFoldMap jsonDiff >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blob pairs.
parseDiffBuilder DiffJSONGraph   = diffGraph >=> serialize Format.JSON
parseDiffBuilder DiffSExpression = distributeFoldMap (doDiff (const id) sexprDiff)
parseDiffBuilder DiffShow        = distributeFoldMap (doDiff (const id) showDiff)
parseDiffBuilder DiffDotGraph    = distributeFoldMap (doDiff (const id) dotGraphDiff)

jsonDiff :: DiffEffects sig m => BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff blobPair = doDiff (const id) (pure . jsonTreeDiff blobPair) blobPair `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

diffGraph :: (Traversable t, DiffEffects sig m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = do
  graph <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList graph
  where
    go :: DiffEffects sig m => BlobPair -> m DiffTreeFileGraph
    go blobPair = doDiff (const id) (pure . jsonGraphDiff blobPair) blobPair
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

type DiffEffects sig m = (Member (Error SomeException) sig, Member (Reader Config) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m)

type Decorate a b = forall term diff . DiffActions term diff => Blob -> term a -> term b


class DOTGraphDiff diff where
  dotGraphDiff :: (Carrier sig m, Member (Reader Config) sig) => diff Loc Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => DOTGraphDiff (Diff syntax) where
  dotGraphDiff = serialize (DOT (diffStyle "diffs")) . renderTreeGraph


class JSONGraphDiff diff where
  jsonGraphDiff :: BlobPair -> diff Loc Loc -> DiffTreeFileGraph

instance (Foldable syntax, Functor syntax, ConstructorName syntax) => JSONGraphDiff (Diff syntax) where
  jsonGraphDiff blobPair diff
    = let graph = renderTreeGraph diff
          toEdge (Edge (a, b)) = defMessage & P.source .~ a^.diffVertexId & P.target .~ b^.diffVertexId
          path = T.pack $ pathForBlobPair blobPair
          lang = bridging # languageForBlobPair blobPair
      in defMessage
           & P.path .~ path
           & P.language .~ lang
           & P.vertices .~ vertexList graph
           & P.edges .~ fmap toEdge (edgeList graph)
           & P.errors .~ mempty


class JSONTreeDiff diff where
  jsonTreeDiff :: BlobPair -> diff Loc Loc -> Rendering.JSON.JSON "diffs" SomeJSON

instance ToJSONFields1 syntax => JSONTreeDiff (Diff syntax) where
  jsonTreeDiff = renderJSONDiff


class SExprDiff diff where
  sexprDiff :: (Carrier sig m, Member (Reader Config) sig) => diff Loc Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => SExprDiff (Diff syntax) where
  sexprDiff = serialize (SExpression ByConstructorName)


class ShowDiff diff where
  showDiff :: (Carrier sig m, Member (Reader Config) sig) => diff Loc Loc -> m Builder

instance Show1 syntax => ShowDiff (Diff syntax) where
  showDiff = serialize Show


class LegacySummarizeDiff term diff | diff -> term, term -> diff where
  legacyDecorateTerm :: Blob -> term Loc -> term (Maybe Declaration)
  legacySummarizeDiff :: BlobPair -> diff (Maybe Declaration) (Maybe Declaration) -> Summaries

instance (Foldable syntax, Functor syntax, HasDeclaration syntax) => LegacySummarizeDiff (Term syntax) (Diff syntax) where
  legacyDecorateTerm = decoratorWithAlgebra . declarationAlgebra
  legacySummarizeDiff = renderToCDiff


class SummarizeDiff term diff | diff -> term, term -> diff where
  decorateTerm :: Blob -> term Loc -> term (Maybe Declaration)
  summarizeDiff :: BlobPair -> diff (Maybe Declaration) (Maybe Declaration) -> TOCSummaryFile

instance (Foldable syntax, Functor syntax, HasDeclaration syntax) => SummarizeDiff (Term syntax) (Diff syntax) where
  decorateTerm = decoratorWithAlgebra . declarationAlgebra
  summarizeDiff blobPair diff = foldr go (defMessage & P.path .~ path & P.language .~ lang) (diffTOC diff)
    where
      path = T.pack $ pathKeyForBlobPair blobPair
      lang = bridging # languageForBlobPair blobPair

      toChangeType = \case
        "added" -> ADDED
        "modified" -> MODIFIED
        "removed" -> REMOVED
        _ -> NONE

      go :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
      go TOCSummary{..} file = defMessage
        & P.path .~ file^.P.path
        & P.language .~ file^.P.language
        & P.changes .~ (defMessage & P.category .~ summaryCategoryName & P.term .~ summaryTermName & P.maybe'span .~ (converting #? summarySpan) & P.changeType .~ toChangeType summaryChangeType) : file^.P.changes
        & P.errors .~ file^.P.errors

      go ErrorSummary{..} file = defMessage
        & P.path .~ file^.P.path
        & P.language .~ file^.P.language
        & P.changes .~ file^.P.changes
        & P.errors .~ (defMessage & P.error .~ errorText & P.maybe'span .~ converting #? errorSpan) : file^.P.errors


type DiffActions term diff =
  ( Bifoldable diff
  , DiffTerms term diff
  , DOTGraphDiff diff
  , JSONGraphDiff diff
  , JSONTreeDiff diff
  , SExprDiff diff
  , ShowDiff diff
  , LegacySummarizeDiff term diff
  , SummarizeDiff term diff
  )

doDiff
  :: DiffEffects sig m
  => Decorate Loc ann
  -> (forall term diff . DiffActions term diff => diff ann ann -> m output)
  -> BlobPair
  -> m output
doDiff decorate render blobPair = do
  SomeTermPair terms <- doParse blobPair decorate
  diff <- diffTerms blobPair terms
  render diff

diffTerms :: (DiffActions term diff, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (term ann) -> m (diff ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  let diff = diffTermPair (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs

doParse :: (Member (Error SomeException) sig, Member Distribute sig, Member Parse sig, Carrier sig m)
  => BlobPair -> Decorate Loc ann -> m (SomeTermPair ann)
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

data SomeTermPair ann where
  SomeTermPair :: DiffActions term diff => Join These (term ann) -> SomeTermPair ann
