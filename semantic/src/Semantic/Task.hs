{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Semantic.Task
( TaskC
, Level(..)
-- * I/O
, Files.readBlob
, Files.readBlobs
, Files.readBlobPairs
, Files.readProject
, Files.findFiles
, Files.write
, Files.FilesArg(..)
-- * Module Resolution
, resolutionMap
, Resolution
-- * Telemetry
, writeLog
, writeStat
, time
, time'
-- * High-level flow
, serialize
-- * Concurrency
, distribute
, distributeFor
, distributeFoldMap
-- * Configuration
, debugOptions
, defaultOptions
, defaultConfig
, terminalFormatter
, logfmtFormatter
-- * Interpreting
, runTask
, runTaskWithOptions
, withOptions
, TaskSession(..)
, runTraceInTelemetry
-- * Re-exports
, Distribute
, Error
, Lift
, throwError
, SomeException(..)
, Telemetry
) where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Trace
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString.Builder
import qualified Data.Flag as Flag
import           Semantic.Config
import           Semantic.Distribute
import           Semantic.Resolution
import qualified Semantic.Task.Files as Files
import           Semantic.Telemetry
import           Semantic.Timeout
import           Serializing.Format hiding (Options)

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type TaskC
  = ResolutionC
  ( Files.FilesC
  ( ReaderC Config
  ( ReaderC TaskSession
  ( TraceInTelemetryC
  ( TelemetryC
  ( ErrorC SomeException
  ( TimeoutC
  ( DistributeC
  ( LiftC IO)))))))))

serialize :: Has (Reader Config) sig m
          => Format input
          -> input
          -> m Builder
serialize format input = do
  formatStyle <- asks (Flag.choose IsTerminal Plain Colourful . configIsTerminal)
  pure (runSerialize formatStyle format input)

data TaskSession
  = TaskSession
  { config    :: Config
  , requestID :: String
  , isPublic  :: Bool
  , logger    :: LogQueue
  , statter   :: StatQueue
  }

-- | Execute a 'TaskC' yielding its result value in 'IO'.
runTask :: TaskSession -> TaskC a -> IO (Either SomeException a)
runTask taskSession@TaskSession{..} task = do
  (result, stat) <- withTiming "run" [] $ do
    let run :: TaskC a -> IO (Either SomeException a)
        run
          = runM
          . withDistribute
          . withTimeout
          . runError
          . runTelemetry logger statter
          . runTraceInTelemetry
          . runReader taskSession
          . runReader config
          . Files.runFiles
          . runResolution
    run task
  queueStat statter stat
  pure result

-- | Execute a 'TaskC' yielding its result value in 'IO' using all default options and configuration.
runTaskWithOptions :: Options -> TaskC a -> IO (Either SomeException a)
runTaskWithOptions options task = withOptions options $ \ config logger statter ->
  runTask (TaskSession config "-" False logger statter) task

-- | Yield config and telemetry queues for options.
withOptions :: Options -> (Config -> LogQueue -> StatQueue -> IO a) -> IO a
withOptions options with = do
  config <- defaultConfig options
  withTelemetry config (\ (TelemetryQueues logger statter _) -> with config logger statter)

runTraceInTelemetry :: TraceInTelemetryC m a
                    -> m a
runTraceInTelemetry = runTraceInTelemetryC

newtype TraceInTelemetryC m a = TraceInTelemetryC { runTraceInTelemetryC :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance Has Telemetry sig m => Algebra (Trace :+: sig) (TraceInTelemetryC m) where
  alg (R other)         = TraceInTelemetryC . alg . handleCoercible $ other
  alg (L (Trace str k)) = writeLog Debug str [] >> k
