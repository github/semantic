{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Semantic.Api.Terms
  ( parseTermBuilder
  , TermOutputFormat(..)
  ) where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Either
import           Data.Foldable (fold)
import           Data.Map.Strict (Map)
import qualified Language.CodeQL as CodeQL
import qualified Language.Go as Go
import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.PHP as PHP
import qualified Language.Python as Python
import qualified Language.Ruby as Ruby
import qualified Language.TSX as TSX
import qualified Language.TypeScript as TypeScript
import           Parsing.Parser
import           Semantic.Config
import           Semantic.Task
import           Serializing.Format hiding (JSON)
import qualified Serializing.SExpression.Precise as SExpr.Precise (serializeSExpression)
import           Source.Language
import           Source.Loc


data TermOutputFormat
  = TermSExpression
  | TermShow
  | TermJSON
  | TermQuiet
  deriving (Eq, Show)

parseTermBuilder :: (Traversable t, Has (Error SomeException) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m)
  => TermOutputFormat -> t Blob -> m Builder
parseTermBuilder TermSExpression = foldMapM (parseWith sexprTermParsers (pure . sexprTerm))
parseTermBuilder TermShow        = foldMapM (parseWith showTermParsers showTerm)
parseTermBuilder TermJSON        = foldMapM (parseWith jsonTermParsers jsonTerm)
parseTermBuilder TermQuiet       = foldMapM quietTerm

foldMapM :: (Traversable t, Monoid out, Applicative m) => (a -> m out) -> t a -> m out
foldMapM f = fmap fold . traverse f

quietTerm :: (Has (Error SomeException) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m) => Blob -> m Builder
quietTerm blob = showTiming <$> time' (parseWith showTermParsers (fmap (const (Right ())) . showTerm) blob `catchError` timingError)
  where
    timingError (SomeException e) = pure (Left (show e))
    showTiming (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show (blobLanguage blob) <> "\t" <> blobFilePath blob <> "\t" <> show duration <> " ms\n")


showTermParsers :: Map Language (SomeParser ShowTerm Loc)
showTermParsers = preciseParsers

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

jsonTermParsers :: Map Language (SomeParser JSONTerm Loc)
jsonTermParsers = preciseParsers

class JSONTerm term where
  jsonTerm :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance JSONTerm Go.Term where
  jsonTerm = serialize Marshal . Go.getTerm

instance JSONTerm Java.Term where
  jsonTerm = serialize Marshal . Java.getTerm

instance JSONTerm JSON.Term where
  jsonTerm = serialize Marshal . JSON.getTerm

instance JSONTerm PHP.Term where
  jsonTerm = serialize Marshal . PHP.getTerm

instance JSONTerm Python.Term where
  jsonTerm = serialize Marshal . Python.getTerm

instance JSONTerm CodeQL.Term where
  jsonTerm = serialize Marshal . CodeQL.getTerm

instance JSONTerm Ruby.Term where
  jsonTerm = serialize Marshal . Ruby.getTerm

instance JSONTerm TSX.Term where
  jsonTerm = serialize Marshal . TSX.getTerm

instance JSONTerm TypeScript.Term where
  jsonTerm = serialize Marshal . TypeScript.getTerm

sexprTermParsers :: Map Language (SomeParser SExprTerm Loc)
sexprTermParsers = preciseParsers

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
