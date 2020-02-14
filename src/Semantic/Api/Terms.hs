{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
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
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Functor.Foldable
import           Data.Graph.Algebraic (Edge (..), edgeList, vertexList)
import           Data.Language
import           Data.Map.Strict (Map)
import           Data.ProtoLens (defMessage)
import           Data.Quieterm
import           Data.Term
import qualified Data.Text as T
import           Parsing.Parser
import           Proto.Semantic as P hiding (Blob)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
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

import qualified Language.Go as GoPrecise
import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.PHP as PHPPrecise
import qualified Language.Python as PythonPrecise
import qualified Language.Ruby as RubyPrecise
import qualified Language.TSX as TSXPrecise
import qualified Language.TypeScript as TypeScriptPrecise


termGraph :: (Traversable t, Has Distribute sig m, Has (Error SomeException) sig m, Has Parse sig m) => t Blob -> m ParseTreeGraphResponse
termGraph blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage
    & P.files .~ toList terms
  where
    go :: (Has (Error SomeException) sig m, Has Parse sig m) => Blob -> m ParseTreeFileGraph
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

parseTermBuilder :: (Traversable t, Has Distribute sig m, Has (Error SomeException) sig m, Has (Reader PerLanguageModes) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m)
  => TermOutputFormat -> t Blob -> m Builder
parseTermBuilder TermJSONTree    = distributeFoldMap jsonTerm >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blobs.
parseTermBuilder TermJSONGraph   = termGraph >=> serialize Format.JSON
parseTermBuilder TermSExpression = distributeFoldMap (\ blob -> asks sexprTermParsers >>= \ parsers -> parseWith parsers (pure . sexprTerm) blob)
parseTermBuilder TermDotGraph    = distributeFoldMap (parseWith dotGraphTermParsers dotGraphTerm)
parseTermBuilder TermShow        = distributeFoldMap (\ blob -> asks showTermParsers >>= \ parsers -> parseWith parsers showTerm blob)
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

jsonTerm :: (Has (Error SomeException) sig m, Has Parse sig m, Has (Reader PerLanguageModes) sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonTerm blob = asks jsonTreeTermParsers >>= \parsers -> parseWith parsers (pure . jsonTreeTerm blob) blob `catchError` jsonError blob

jsonError :: Applicative m => Blob -> SomeException -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonError blob (SomeException e) = pure $ renderJSONError blob (show e)

quietTerm :: (Has (Error SomeException) sig m, Has (Reader PerLanguageModes) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m) => Blob -> m Builder
quietTerm blob = showTiming blob <$> time' ( asks showTermParsers >>= \ parsers -> parseWith parsers (fmap (const (Right ())) . showTerm) blob `catchError` timingError )
  where
    timingError (SomeException e) = pure (Left (show e))
    showTiming Blob{..} (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show (blobLanguage blob) <> "\t" <> blobPath blob <> "\t" <> show duration <> " ms\n")


showTermParsers :: PerLanguageModes -> Map Language (SomeParser ShowTerm Loc)
showTermParsers = allParsers

class ShowTerm term where
  showTerm :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance (TermMode term ~ strategy, ShowTermBy strategy term) => ShowTerm term where
  showTerm = showTermBy @strategy

class ShowTermBy (strategy :: LanguageMode) term where
  showTermBy :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance ShowTermBy 'Precise GoPrecise.Term where
  showTermBy = serialize Show . void . GoPrecise.getTerm

instance ShowTermBy 'Precise Java.Term where
  showTermBy = serialize Show . void . Java.getTerm

instance ShowTermBy 'Precise JSON.Term where
  showTermBy = serialize Show . void . JSON.getTerm

instance ShowTermBy 'Precise PHPPrecise.Term where
  showTermBy = serialize Show . void . PHPPrecise.getTerm

instance ShowTermBy 'Precise PythonPrecise.Term where
  showTermBy = serialize Show . void . PythonPrecise.getTerm

instance ShowTermBy 'Precise RubyPrecise.Term where
  showTermBy = serialize Show . void . RubyPrecise.getTerm

instance ShowTermBy 'Precise TSXPrecise.Term where
  showTermBy = serialize Show . void . TSXPrecise.getTerm

instance ShowTermBy 'Precise TypeScriptPrecise.Term where
  showTermBy = serialize Show . void . TypeScriptPrecise.getTerm

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

instance SExprTermBy 'Precise GoPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . GoPrecise.getTerm

instance SExprTermBy 'Precise Java.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Java.getTerm

instance SExprTermBy 'Precise JSON.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . JSON.getTerm

instance SExprTermBy 'Precise PHPPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . PHPPrecise.getTerm

instance SExprTermBy 'Precise PythonPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . PythonPrecise.getTerm

instance SExprTermBy 'Precise RubyPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . RubyPrecise.getTerm

instance SExprTermBy 'Precise TSXPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . TSXPrecise.getTerm

instance SExprTermBy 'Precise TypeScriptPrecise.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . TypeScriptPrecise.getTerm

instance (Recursive (term Loc), SExpr.ToSExpression (Base (term Loc))) => SExprTermBy 'ALaCarte term where
  sexprTermBy = SExpr.serializeSExpression ByConstructorName


dotGraphTermParsers :: Map Language (SomeParser DOTGraphTerm Loc)
dotGraphTermParsers = aLaCarteParsers

class DOTGraphTerm term where
  dotGraphTerm :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance (Recursive (term Loc), ToTreeGraph TermVertex (Base (term Loc))) => DOTGraphTerm term where
  dotGraphTerm = serialize (DOT (termStyle "terms")) . renderTreeGraph


jsonTreeTermParsers :: PerLanguageModes -> Map Language (SomeParser JSONTreeTerm Loc)
jsonTreeTermParsers = allParsers

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
