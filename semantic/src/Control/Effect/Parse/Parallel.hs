{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Control.Effect.Parse.Parallel
  ( ParallelParse (..)
  , parseWith
  , ParserMap
  ) where

import AST.Unmarshal
import Control.Algebra
import Control.Effect.Parse (Has, ParserMap)
import Control.Exception
import Data.Blob
import Data.Kind (Type)

data ParallelParse (m :: Type -> Type) k where
  ParseWith ::
    Traversable t =>
    ParserMap c ann ->
    (forall term. c term => term ann -> IO a) ->
    t Blob ->
    ParallelParse m (t (Either SomeException a))

parseWith ::
    (Has ParallelParse sig m, Traversable t) =>
    ParserMap c ann ->
    (forall term. c term => term ann -> IO a) ->
    t Blob ->
    m (t (Either SomeException a))
parseWith pmap fn blobs = send (ParseWith pmap fn blobs)
