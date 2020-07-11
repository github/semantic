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
  | TermQuiet
  deriving (Eq, Show)

parseTermBuilder :: (Traversable t, Has Distribute sig m, Has (Error SomeException) sig m, Has Parse sig m, Has (Reader Config) sig m, MonadIO m)
  => TermOutputFormat -> t Blob -> m Builder
parseTermBuilder TermSExpression = distributeFoldMap (parseWith sexprTermParsers (pure . sexprTerm))
parseTermBuilder TermShow        = distributeFoldMap (parseWith showTermParsers showTerm)
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

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

instance (ShowTermBy term) => ShowTerm term where
  showTerm = showTermBy @term

class ShowTermBy term where
  showTermBy :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance ShowTermBy Go.Term where
  showTermBy = serialize Show . void . Go.getTerm

instance ShowTermBy Java.Term where
  showTermBy = serialize Show . void . Java.getTerm

instance ShowTermBy JSON.Term where
  showTermBy = serialize Show . void . JSON.getTerm

instance ShowTermBy PHP.Term where
  showTermBy = serialize Show . void . PHP.getTerm

instance ShowTermBy Python.Term where
  showTermBy = serialize Show . void . Python.getTerm

instance ShowTermBy CodeQL.Term where
  showTermBy = serialize Show . void . CodeQL.getTerm

instance ShowTermBy Ruby.Term where
  showTermBy = serialize Show . void . Ruby.getTerm

instance ShowTermBy TSX.Term where
  showTermBy = serialize Show . void . TSX.getTerm

instance ShowTermBy TypeScript.Term where
  showTermBy = serialize Show . void . TypeScript.getTerm

sexprTermParsers :: Map Language (SomeParser SExprTerm Loc)
sexprTermParsers = preciseParsers

class SExprTerm term where
  sexprTerm :: term Loc -> Builder

instance (SExprTermBy term) => SExprTerm term where
  sexprTerm = sexprTermBy @term

class SExprTermBy term where
  sexprTermBy :: term Loc -> Builder

instance SExprTermBy Go.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Go.getTerm

instance SExprTermBy Java.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Java.getTerm

instance SExprTermBy JSON.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . JSON.getTerm

instance SExprTermBy PHP.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . PHP.getTerm

instance SExprTermBy Python.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Python.getTerm

instance SExprTermBy CodeQL.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . CodeQL.getTerm

instance SExprTermBy Ruby.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Ruby.getTerm

instance SExprTermBy TSX.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . TSX.getTerm

instance SExprTermBy TypeScript.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . TypeScript.getTerm
