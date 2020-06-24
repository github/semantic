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
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Language
import Data.Map.Strict (Map)
import Data.Quieterm
import Data.Term
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
showTermParsers = preciseParsers

class ShowTerm term where
  showTerm :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance (TermMode term ~ strategy, ShowTermBy strategy term) => ShowTerm term where
  showTerm = showTermBy @strategy

class ShowTermBy (strategy :: LanguageMode) term where
  showTermBy :: (Has (Reader Config) sig m) => term Loc -> m Builder

instance ShowTermBy 'Precise Go.Term where
  showTermBy = serialize Show . void . Go.getTerm

instance ShowTermBy 'Precise Java.Term where
  showTermBy = serialize Show . void . Java.getTerm

instance ShowTermBy 'Precise JSON.Term where
  showTermBy = serialize Show . void . JSON.getTerm

instance ShowTermBy 'Precise PHP.Term where
  showTermBy = serialize Show . void . PHP.getTerm

instance ShowTermBy 'Precise Python.Term where
  showTermBy = serialize Show . void . Python.getTerm

instance ShowTermBy 'Precise CodeQL.Term where
  showTermBy = serialize Show . void . CodeQL.getTerm

instance ShowTermBy 'Precise Ruby.Term where
  showTermBy = serialize Show . void . Ruby.getTerm

instance ShowTermBy 'Precise TSX.Term where
  showTermBy = serialize Show . void . TSX.getTerm

instance ShowTermBy 'Precise TypeScript.Term where
  showTermBy = serialize Show . void . TypeScript.getTerm

instance (Recursive (term Loc), Show1 syntax, Base (term Loc) ~ TermF syntax Loc) => ShowTermBy 'ALaCarte term where
  showTermBy = serialize Show . quieterm


sexprTermParsers :: Map Language (SomeParser SExprTerm Loc)
sexprTermParsers = preciseParsers

class SExprTerm term where
  sexprTerm :: term Loc -> Builder

instance (TermMode term ~ strategy, SExprTermBy strategy term) => SExprTerm term where
  sexprTerm = sexprTermBy @strategy

class SExprTermBy (strategy :: LanguageMode) term where
  sexprTermBy :: term Loc -> Builder

instance SExprTermBy 'Precise Go.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Go.getTerm

instance SExprTermBy 'Precise Java.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Java.getTerm

instance SExprTermBy 'Precise JSON.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . JSON.getTerm

instance SExprTermBy 'Precise PHP.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . PHP.getTerm

instance SExprTermBy 'Precise Python.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Python.getTerm

instance SExprTermBy 'Precise CodeQL.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . CodeQL.getTerm

instance SExprTermBy 'Precise Ruby.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . Ruby.getTerm

instance SExprTermBy 'Precise TSX.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . TSX.getTerm

instance SExprTermBy 'Precise TypeScript.Term where
  sexprTermBy = SExpr.Precise.serializeSExpression . TypeScript.getTerm
