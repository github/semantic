{-# LANGUAGE ConstraintKinds, MonoLocalBinds, RankNTypes #-}
module Semantic.Api.Terms
  ( termGraph
  , parseTermBuilder
  , TermOutputFormat(..)
  ) where

import           Analysis.ConstructorName (ConstructorName)
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Either
import           Data.Graph
import           Data.JSON.Fields
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Quieterm
import           Data.Term
import qualified Data.Text as T
import           Parsing.Parser
import           Prologue
import           Proto.Semantic as P hiding (Blob)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON()
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import           Semantic.Api.Bridge
import           Semantic.Config
-- import           Semantic.Proto.SemanticPB hiding (Blob)
import           Semantic.Task
import           Serializing.Format hiding (JSON)
import qualified Serializing.Format as Format
import           Source.Loc

import qualified Language.Python as Py


termGraph :: (Traversable t, Member Distribute sig, ParseEffects sig m) => t Blob -> m ParseTreeGraphResponse
termGraph blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage
    & P.files .~ toList terms
  where
    go :: ParseEffects sig m => Blob -> m ParseTreeFileGraph
    go blob = doParse (pure . jsonGraphTerm blob) blob
      `catchError` \(SomeException e) ->
        pure $ defMessage
          & P.path .~ path
          & P.language .~ lang
          & P.vertices .~ mempty
          & P.edges .~ mempty
          & P.errors .~ [defMessage & P.error .~ T.pack (show e)]
      where
        path = T.pack $ blobPath blob
        lang = bridging # blobLanguage blob

data TermOutputFormat
  = TermJSONTree
  | TermJSONGraph
  | TermSExpression
  | TermDotGraph
  | TermShow
  | TermQuiet
  deriving (Eq, Show)

parseTermBuilder :: (Traversable t, Member Distribute sig, ParseEffects sig m, MonadIO m)
  => TermOutputFormat -> t Blob -> m Builder
parseTermBuilder TermJSONTree    = distributeFoldMap jsonTerm >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blobs.
parseTermBuilder TermJSONGraph   = termGraph >=> serialize Format.JSON
parseTermBuilder TermSExpression = distributeFoldMap (doParse sexprTerm)
parseTermBuilder TermDotGraph    = distributeFoldMap (doParse dotGraphTerm)
parseTermBuilder TermShow        = distributeFoldMap (doParse showTerm)
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

jsonTerm :: ParseEffects sig m => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonTerm blob = doParse (pure . jsonTreeTerm blob) blob `catchError` jsonError blob

jsonError :: Applicative m => Blob -> SomeException -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonError blob (SomeException e) = pure $ renderJSONError blob (show e)

quietTerm :: (ParseEffects sig m, MonadIO m) => Blob -> m Builder
quietTerm blob = showTiming blob <$> time' ( doParse (fmap (const (Right ())) . showTerm) blob `catchError` timingError )
  where
    timingError (SomeException e) = pure (Left (show e))
    showTiming Blob{..} (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show (blobLanguage blob) <> "\t" <> blobPath blob <> "\t" <> show duration <> " ms\n")


type ParseEffects sig m = (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Member (Reader Config) sig, Carrier sig m)


class ShowTerm term where
  showTerm :: (Carrier sig m, Member (Reader Config) sig) => term Loc -> m Builder

instance (Functor syntax, Show1 syntax) => ShowTerm (Term syntax) where
  showTerm = serialize Show . quieterm

instance ShowTerm Py.Term where
  showTerm = serialize Show . Py.getTerm


class SExprTerm term where
  sexprTerm :: (Carrier sig m, Member (Reader Config) sig) => term Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => SExprTerm (Term syntax) where
  sexprTerm = serialize (SExpression ByConstructorName)


class DOTGraphTerm term where
  dotGraphTerm :: (Carrier sig m, Member (Reader Config) sig) => term Loc -> m Builder

instance (ConstructorName syntax, Foldable syntax, Functor syntax) => DOTGraphTerm (Term syntax) where
  dotGraphTerm = serialize (DOT (termStyle "terms")) . renderTreeGraph


class JSONTreeTerm term where
  jsonTreeTerm :: Blob -> term Loc -> Rendering.JSON.JSON "trees" SomeJSON

instance ToJSONFields1 syntax => JSONTreeTerm (Term syntax) where
  jsonTreeTerm = renderJSONTerm


class JSONGraphTerm term where
  jsonGraphTerm :: Blob -> term Loc -> ParseTreeFileGraph

instance (Foldable syntax, Functor syntax, ConstructorName syntax) => JSONGraphTerm (Term syntax) where
  jsonGraphTerm blob t
    = let graph = renderTreeGraph t
          toEdge (Edge (a, b)) = defMessage & P.source .~ a^.vertexId & P.target .~ b^.vertexId
          path = T.pack $ blobPath blob
          lang = bridging # blobLanguage blob
      in defMessage
          & P.path .~ path
          & P.language .~ lang
          & P.vertices .~ vertexList graph
          & P.edges .~ fmap toEdge (edgeList graph)
          & P.errors .~ mempty

type TermActions t = (DOTGraphTerm t, JSONGraphTerm t, JSONTreeTerm t, SExprTerm t, ShowTerm t)

doParse
  :: ( Carrier sig m
     , Member (Error SomeException) sig
     , Member Parse sig
     )
  => (forall term . TermActions term => term Loc -> m a)
  -> Blob
  -> m a
doParse with blob = case blobLanguage blob of
  Go         -> parse goParser         blob >>= with
  Haskell    -> parse haskellParser    blob >>= with
  JavaScript -> parse tsxParser        blob >>= with
  JSON       -> parse jsonParser       blob >>= with
  JSX        -> parse tsxParser        blob >>= with
  Markdown   -> parse markdownParser   blob >>= with
  Python     -> parse pythonParser     blob >>= with
  Ruby       -> parse rubyParser       blob >>= with
  TypeScript -> parse typescriptParser blob >>= with
  TSX        -> parse tsxParser        blob >>= with
  PHP        -> parse phpParser        blob >>= with
  _          -> noLanguageForBlob (blobPath blob)
