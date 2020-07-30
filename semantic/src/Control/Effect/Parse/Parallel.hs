{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Control.Effect.Parse.Parallel
  ( ParallelParse (..)
  , parseWith
  , ParserMap
  , ParseError (..)
  , errorBlob
  ) where

import AST.Unmarshal
import Control.Algebra
import Control.Effect.Parse (Has, ParserMap)
import Data.Blob
import Data.Kind (Type)
import Parsing.TreeSitter

data ParseError
  = TSError TSParseException Blob
  | NoLanguage Blob
    deriving (Show, Eq)

errorBlob :: ParseError -> Blob
errorBlob = \case
  TSError _ b -> b
  NoLanguage b -> b

data ParallelParse (m :: Type -> Type) k where
  ParseWith ::
    Traversable t =>
    ParserMap c ann ->
    (forall term. c term => Blob -> term ann -> IO a) ->
    t Blob ->
    ParallelParse m (t (Either ParseError a))

parseWith ::
    (Has ParallelParse sig m, Traversable t) =>
    ParserMap c ann ->
    (forall term. c term => Blob -> term ann -> IO a) ->
    t Blob ->
    m (t (Either ParseError a))
parseWith pmap fn blobs = send (ParseWith pmap fn blobs)
