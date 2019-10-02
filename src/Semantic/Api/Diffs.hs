{-# LANGUAGE GADTs, ConstraintKinds, TypeOperators, RankNTypes, UndecidableInstances #-}
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
parseDiffBuilder DiffSExpression = distributeFoldMap (doDiff (const id) sexprDiff)
parseDiffBuilder DiffShow        = distributeFoldMap (doDiff (const id) showDiff)
parseDiffBuilder DiffDotGraph    = distributeFoldMap dotGraphDiff

type RenderJSON m syntax = forall syntax . DiffActions syntax => BlobPair -> Diff syntax Loc Loc -> m (Rendering.JSON.JSON "diffs" SomeJSON)

jsonDiff :: DiffEffects sig m => RenderJSON m syntax -> BlobPair -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonDiff f blobPair = doDiff (const id) (f blobPair) blobPair `catchError` jsonError blobPair

jsonError :: Applicative m => BlobPair -> SomeException -> m (Rendering.JSON.JSON "diffs" SomeJSON)
jsonError blobPair (SomeException e) = pure $ renderJSONDiffError blobPair (show e)

renderJSONTree :: (Applicative m, ToJSONFields1 syntax) => BlobPair -> Diff syntax Loc Loc -> m (Rendering.JSON.JSON "diffs" SomeJSON)
renderJSONTree blobPair = pure . renderJSONDiff blobPair

diffGraph :: (Traversable t, DiffEffects sig m) => t BlobPair -> m DiffTreeGraphResponse
diffGraph blobs = DiffTreeGraphResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: DiffEffects sig m => BlobPair -> m DiffTreeFileGraph
    go blobPair = doDiff (const id) render blobPair
      `catchError` \(SomeException e) ->
        pure (DiffTreeFileGraph path lang mempty mempty (V.fromList [ParseError (T.pack (show e))]))
      where
        path = T.pack $ pathForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair

        render :: (Foldable syntax, Functor syntax, ConstructorName syntax, Applicative m) => Diff syntax Loc Loc -> m DiffTreeFileGraph
        render diff =
          let graph = renderTreeGraph diff
              toEdge (Edge (a, b)) = DiffTreeEdge (diffVertexId a) (diffVertexId b)
          in pure $ DiffTreeFileGraph path lang (V.fromList (vertexList graph)) (V.fromList (fmap toEdge (edgeList graph))) mempty

dotGraphDiff :: DiffEffects sig m => BlobPair -> m Builder
dotGraphDiff = doDiff (const id) render
  where render :: (Carrier sig m, ConstructorName syntax, Foldable syntax, Functor syntax, Member (Reader Config) sig) => Diff syntax Loc Loc -> m Builder
        render = serialize (DOT (diffStyle "diffs")) . renderTreeGraph

type DiffEffects sig m = (Member (Error SomeException) sig, Member (Reader Config) sig, Member Telemetry sig, Member Distribute sig, Member Parse sig, Carrier sig m, MonadIO m)

type Decorate a b = forall syntax . DiffActions syntax => Blob -> Term syntax a -> Term syntax b

class SExprDiff diff where
  sexprDiff :: (Carrier sig m, Member (Reader Config) sig) => diff Loc Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => SExprDiff (Diff syntax) where
  sexprDiff = serialize (SExpression ByConstructorName)

class ShowDiff diff where
  showDiff :: (Carrier sig m, Member (Reader Config) sig) => diff Loc Loc -> m Builder

instance Show1 syntax => ShowDiff (Diff syntax) where
  showDiff = serialize Show

class ( ConstructorName t
      , Diffable t
      , Eq1 t
      , HasDeclaration t
      , Hashable1 t
      , Show1 t
      , Traversable t
      , ToJSONFields1 t
      )
   => DiffActions t
instance ( ConstructorName t
         , Diffable t
         , Eq1 t
         , HasDeclaration t
         , Hashable1 t
         , Show1 t
         , Traversable t
         , ToJSONFields1 t
         )
      => DiffActions t

doDiff
  :: DiffEffects sig m
  => Decorate Loc ann
  -> (forall syntax . DiffActions syntax => Diff syntax ann ann -> m output)
  -> BlobPair
  -> m output
doDiff decorate render blobPair = do
  SomeTermPair terms <- doParse blobPair decorate
  diff <- diffTerms blobPair terms
  render diff

diffTerms :: (DiffActions syntax, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair -> Join These (Term syntax ann) -> m (Diff syntax ann ann)
diffTerms blobs terms = time "diff" languageTag $ do
  let diff = diffTermPair (runJoin terms)
  diff <$ writeStat (Stat.count "diff.nodes" (bilength diff) languageTag)
  where languageTag = languageTagForBlobPair blobs

doParse :: (Member (Error SomeException) sig, Member Distribute sig, Member Parse sig, Carrier sig m)
  => BlobPair -> Decorate Loc ann -> m (SomeTermPair DiffActions ann)
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
  SomeTermPair :: typeclasses syntax => Join These (Term syntax ann) -> SomeTermPair typeclasses ann

withSomeTermPair :: (forall syntax . typeclasses syntax => Join These (Term syntax ann) -> a) -> SomeTermPair typeclasses ann -> a
withSomeTermPair with (SomeTermPair terms) = with terms
