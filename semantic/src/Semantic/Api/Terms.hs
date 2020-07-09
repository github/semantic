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
  ( parseTermBuilder
  , TermOutputFormat(..)
  ) where

import Control.Effect.Error
import Control.Effect.Parse
import Control.Effect.Reader
import Control.Monad
import Control.Monad.IO.Class
import Data.Blob
import Data.ByteString.Builder
import Data.Either
import Data.Map.Strict (Map)
import qualified Language.CodeQL as CodeQL
import qualified Language.Go as Go
import qualified Language.JSON as JSON
import qualified Language.Java as Java
import qualified Language.PHP as PHP
import qualified Language.Python as Python
import qualified Language.Ruby as Ruby
import qualified Language.TSX as TSX
import qualified Language.TypeScript as TypeScript
import Parsing.Parser
import Semantic.Config
import Semantic.Task
import Serializing.Format hiding (JSON)
import qualified Serializing.SExpression.Precise as SExpr.Precise (serializeSExpression)
import Source.Language
import Source.Loc


data TermOutputFormat
  = TermSExpression
  | TermShow
  | TermQuiet
  deriving (Eq, Show)

parseTermBuilder :: (Traversable t, Has Distribute sig m, Has (Error SomeException) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m)
  => TermOutputFormat -> t Blob -> m Builder
parseTermBuilder TermSExpression = distributeFoldMap (parseWith sexprTermParsers (pure . sexprTerm))
parseTermBuilder TermShow        = distributeFoldMap (parseWith showTermParsers showTerm)
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

quietTerm :: (Has (Error SomeException) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m) => Blob -> m Builder
quietTerm blob = showTiming blob <$> time' (parseWith showTermParsers (fmap (const (Right ())) . showTerm) blob `catchError` timingError)
  where
    timingError (SomeException e) = pure (Left (show e))
    showTiming Blob{..} (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show (blobLanguage blob) <> "\t" <> blobFilePath blob <> "\t" <> show duration <> " ms\n")


showTermParsers :: Map Language (SomeParser ShowTerm Loc)
showTermParsers = allParsers

class ShowTerm term where
  showTerm :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance ShowTerm Go.Term where
  showTerm = serialize Show . void . Go.getTerm

instance ShowTerm Java.Term where
  showTerm = serialize Show . void . Java.getTerm

instance ShowTerm JSON.Term where
  showTerm = serialize Show . void . JSON.getTerm

instance ShowTerm PHP.Term where
  showTerm = serialize Show . void . PHP.getTerm

instance ShowTerm Python.Term where
  showTerm = serialize Show . void . Python.getTerm

instance ShowTerm CodeQL.Term where
  showTerm = serialize Show . void . CodeQL.getTerm

instance ShowTerm Ruby.Term where
  showTerm = serialize Show . void . Ruby.getTerm

instance ShowTerm TSX.Term where
  showTerm = serialize Show . void . TSX.getTerm

instance ShowTerm TypeScript.Term where
  showTerm = serialize Show . void . TypeScript.getTerm

sexprTermParsers :: Map Language (SomeParser SExprTerm Loc)
sexprTermParsers = allParsers

class SExprTerm term where
  sexprTerm :: term Loc -> Builder

instance SExprTerm Go.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . Go.getTerm

instance SExprTerm Java.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . Java.getTerm

instance SExprTerm JSON.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . JSON.getTerm

instance SExprTerm PHP.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . PHP.getTerm

instance SExprTerm Python.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . Python.getTerm

instance SExprTerm CodeQL.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . CodeQL.getTerm

instance SExprTerm Ruby.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . Ruby.getTerm

instance SExprTerm TSX.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . TSX.getTerm

instance SExprTerm TypeScript.Term where
  sexprTerm = SExpr.Precise.serializeSExpression . TypeScript.getTerm
