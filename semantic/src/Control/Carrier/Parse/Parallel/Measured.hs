{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for parallel 'Parse' effects suitable for use in production.
module Control.Carrier.Parse.Parallel.Measured
  ( -- * Parse carrier
    ParseC (..),
    ParseError (..),
    errorBlob
  )
where

import Control.Algebra
import Control.Concurrent.Async
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Parse.Parallel
import Control.Effect.Reader
import Control.Effect.Trace
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Blob
import qualified Data.Flag as Flag
import qualified Data.Map.Strict as Map
import Parsing.Parser
import Parsing.TreeSitter
import Source.Loc
import Semantic.Config
import Semantic.Task (TaskSession (..))
import Semantic.Telemetry

newtype ParseC m a = ParseC {runParse :: m a}
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance
  ( Has (Reader TaskSession) sig m,
    Has Telemetry sig m,
    Has Trace sig m,
    Has (Lift IO) sig m,
    MonadIO m
  ) =>
  Algebra (ParallelParse :+: sig) (ParseC m)
  where
  alg hdl sig ctx = case sig of
    L (ParseWith pmap fn blobs) -> (<$ ctx) <$> runParser pmap fn blobs
    R other -> ParseC (alg (runParse . hdl) other ctx)
-- | Parse a 'Blob' in 'IO'.
runParser ::
  ( Has (Reader TaskSession) sig m,
    Has Telemetry sig m,
    Has Trace sig m,
    MonadIO m,
    Traversable t
  ) =>
  ParserMap c ann ->
  (forall term . c term => Blob -> term ann -> IO a) ->
  t Blob ->
  m (t (Either ParseError a))
runParser pmap fn blobs = do
  config <- asks config
  liftIO . forConcurrently blobs $ \blob -> do
    case Map.lookup (blobLanguage blob) pmap of
      Just (SomeParser ((UnmarshalParser language) :: Parser (term ann))) -> do
        res <- parseToPreciseAST @term (configTreeSitterParseTimeout config) (configTreeSitterUnmarshalTimeout config) language blob
        case res of
          Left err -> pure (Left (TSError err blob))
          Right val -> Right <$> fn blob val
      _ -> do
        pure (Left (NoLanguage blob))
