{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Semantic.Api.Terms
  ( termGraph
  , parseTermBuilder
  , TermOutputFormat(..)
  ) where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON)
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Either
import           Data.Graph
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
import           Semantic.Task
import           Serializing.Format hiding (JSON)
import qualified Serializing.Format as Format
import qualified Serializing.SExpression as SExpr
import qualified Serializing.SExpression.Precise as SExpr.Precise (serializeSExpression)
import           Source.Loc

import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.Python as PythonPrecise


termGraph :: (Traversable t, Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Carrier sig m) => t Blob -> m ParseTreeGraphResponse
termGraph blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage
    & P.files .~ toList terms
  where
    go :: (Member (Error SomeException) sig, Member Parse sig, Carrier sig m) => Blob -> m ParseTreeFileGraph
    go blob = parseWith jsonGraphTermParsers (pure . jsonGraphTerm blob) blob
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

parseTermBuilder :: (Traversable t, Member Distribute sig, Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Member (Reader Config) sig, Carrier sig m, MonadIO m)
  => TermOutputFormat -> t Blob -> m Builder
parseTermBuilder TermJSONTree    = distributeFoldMap jsonTerm >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blobs.
parseTermBuilder TermJSONGraph   = termGraph >=> serialize Format.JSON
parseTermBuilder TermSExpression = distributeFoldMap (\ blob -> asks sexprTermParsers >>= \ parsers -> parseWith parsers (pure . sexprTerm) blob)
parseTermBuilder TermDotGraph    = distributeFoldMap (parseWith dotGraphTermParsers dotGraphTerm)
parseTermBuilder TermShow        = distributeFoldMap (\ blob -> asks showTermParsers >>= \ parsers -> parseWith parsers showTerm blob)
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

jsonTerm :: (Member (Error SomeException) sig, Member Parse sig, Carrier sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonTerm blob = parseWith jsonTreeTermParsers (pure . jsonTreeTerm blob) blob `catchError` jsonError blob

jsonError :: Applicative m => Blob -> SomeException -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonError blob (SomeException e) = pure $ renderJSONError blob (show e)

quietTerm :: (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Member (Reader Config) sig, Carrier sig m, MonadIO m) => Blob -> m Builder
quietTerm blob = showTiming blob <$> time' ( asks showTermParsers >>= \ parsers -> parseWith parsers (fmap (const (Right ())) . showTerm) blob `catchError` timingError )
  where
    timingError (SomeException e) = pure (Left (show e))
    showTiming Blob{..} (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show (blobLanguage blob) <> "\t" <> blobPath blob <> "\t" <> show duration <> " ms\n")


showTermParsers :: PerLanguageModes -> Map Language (SomeParser ShowTerm Loc)
showTermParsers = allParsers

class ShowTerm term where
  showTerm :: (Carrier sig m, Member (Reader Config) sig) => term Loc -> m Builder

instance (TermMode term ~ strategy, ShowTermBy strategy term) => ShowTerm term where
  showTerm = showTermBy @strategy

class ShowTermBy (strategy :: LanguageMode) term where
  showTermBy :: (Carrier sig m, Member (Reader Config) sig) => term Loc -> m Builder

instance ShowTermBy 'Precise Java.Term where
  showTermBy = serialize Show . void . Java.getTerm

instance ShowTermBy 'Precise JSON.Term where
  showTermBy = serialize Show . void . JSON.getTerm

instance ShowTermBy 'Precise PythonPrecise.Term where
  showTermBy = serialize Show . void . PythonPrecise.getTerm

instance (Recursive (term Loc), Show1 syntax, Base (term Loc) ~ TermF syntax Loc) => ShowTermBy 'ALaCarte term where
  showTermBy = serialize Show . quieterm


sexprTermParsers :: PerLanguageModes -> Map Language (SomeParser SExprTerm Loc)
sexprTermParsers = allParsers

class SExprTerm term where
  sexprTerm :: term Loc -> Builder

instance (TermMode term ~ strategy, SExprTermBy strategy term) => SExprTerm term where
  sexprTerm = sexprTermBy @strategy

class SExprTermBy (strategy :: LanguageMode) term where
  sexprTermBy :: term Loc -> Builder

instance SExprTermBy 'Precise Java.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Java.getTerm

instance SExprTermBy 'Precise JSON.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . JSON.getTerm

instance SExprTermBy 'Precise PythonPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . PythonPrecise.getTerm

instance (Recursive (term Loc), SExpr.ToSExpression (Base (term Loc))) => SExprTermBy 'ALaCarte term where
  sexprTermBy = SExpr.serializeSExpression ByConstructorName


dotGraphTermParsers :: Map Language (SomeParser DOTGraphTerm Loc)
dotGraphTermParsers = aLaCarteParsers

class DOTGraphTerm term where
  dotGraphTerm :: (Carrier sig m, Member (Reader Config) sig) => term Loc -> m Builder

instance (Recursive (term Loc), ToTreeGraph TermVertex (Base (term Loc))) => DOTGraphTerm term where
  dotGraphTerm = serialize (DOT (termStyle "terms")) . renderTreeGraph


jsonTreeTermParsers :: Map Language (SomeParser JSONTreeTerm Loc)
jsonTreeTermParsers = aLaCarteParsers

class JSONTreeTerm term where
  jsonTreeTerm :: Blob -> term Loc -> Rendering.JSON.JSON "trees" SomeJSON

instance ToJSON (term Loc) => JSONTreeTerm term where
  jsonTreeTerm = renderJSONTerm


jsonGraphTermParsers :: Map Language (SomeParser JSONGraphTerm Loc)
jsonGraphTermParsers = aLaCarteParsers

class JSONGraphTerm term where
  jsonGraphTerm :: Blob -> term Loc -> ParseTreeFileGraph

instance (Recursive (term Loc), ToTreeGraph TermVertex (Base (term Loc))) => JSONGraphTerm term where
  jsonGraphTerm blob t
    = let graph = renderTreeGraph t
          toEdge (Edge (a, b)) = defMessage & P.source .~ a^.vertexId & P.target .~ b^.vertexId
          path = T.pack $ blobPath blob
          lang = bridging # blobLanguage blob
      in defMessage
          & P.path     .~ path
          & P.language .~ lang
          & P.vertices .~ vertexList graph
          & P.edges    .~ fmap toEdge (edgeList graph)
          & P.errors   .~ mempty
