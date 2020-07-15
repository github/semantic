{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Semantic.Telemetry
(
  -- Async telemetry interface
  withLogger
, withErrorReporter
, withStatter
, LogQueue
, StatQueue
, ErrorQueue
, TelemetryQueues(..)
, queueLogMessage
, queueErrorReport
, queueStat

-- Create stats
, Stat.increment
, Stat.decrement
, Stat.count
, Stat.gauge
, Stat.timing
, Stat.withTiming
, Stat.histogram
, Stat.set

-- Statsd client
, statsClient
, StatsClient

-- Error reporters
, nullErrorReporter

-- Logging options and formatters
, Level(..)
, LogOptions(..)
, logfmtFormatter
, terminalFormatter
, LogFormatter

-- Eff interface for telemetry
, writeLog
, writeStat
, time
, time'
, Telemetry(..)
, runTelemetry
, TelemetryC(..)
, ignoreTelemetry
, IgnoreTelemetryC(..)
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.LocalTime as LocalTime
import           Semantic.Telemetry.AsyncQueue
import           Semantic.Telemetry.Error
import           Semantic.Telemetry.Log
import           Semantic.Telemetry.Stat as Stat

type LogQueue = AsyncQueue Message LogOptions
type StatQueue = AsyncQueue Stat StatsClient
type ErrorQueue = AsyncQueue ErrorReport ErrorReporter

data TelemetryQueues
  = TelemetryQueues
  { telemetryLogger        :: LogQueue
  , telemetryStatter       :: StatQueue
  , telemetryErrorReporter :: ErrorQueue
  }

-- | Execute an action in IO with access to a logger (async log queue).
withLogger :: LogOptions         -- ^ Log options
           -> Int                -- ^ Max stats queue size before dropping stats
           -> (LogQueue -> IO c) -- ^ Action in IO
           -> IO c
withLogger options size = bracket setup closeAsyncQueue
  where setup = newAsyncQueue size writeLogMessage options

-- | Execute an action in IO with access to an error reporter (async error reporting queue).
withErrorReporter :: IO ErrorReporter -> Int -> (ErrorQueue -> IO c) -> IO c
withErrorReporter errorReporter size = bracket setup closeAsyncQueue
  where setup = errorReporter >>= newAsyncQueue size ($)

-- | Execute an action in IO with access to a statter (async stat queue).
-- Handles the bracketed setup and teardown of the underlying 'AsyncQueue' and
-- 'StatsClient'.
withStatter :: Host                -- ^ Statsd host
            -> Port                -- ^ Statsd port
            -> Namespace           -- ^ Namespace prefix for stats
            -> Int                 -- ^ Max stats queue size before dropping stats
            -> (StatQueue -> IO c) -- ^ Action in IO
            -> IO c
withStatter host port ns size = bracket setup teardown
  where setup = statsClient host port ns >>= newAsyncQueue size sendStat
        teardown statter = closeAsyncQueue statter >> Stat.closeStatClient (asyncQueueExtra statter)

-- | Queue a message to be logged.
queueLogMessage :: MonadIO io => LogQueue -> Level -> String -> [(String, String)] -> io ()
queueLogMessage q@AsyncQueue{..} level message pairs
  | Just logLevel <- logOptionsLevel asyncQueueExtra
  , level <= logLevel = liftIO Time.getCurrentTime >>= liftIO . LocalTime.utcToLocalZonedTime >>= liftIO . writeAsyncQueue q . Message level message pairs
  | otherwise = pure ()

-- | Queue an error to be reported.
queueErrorReport :: MonadIO io => ErrorQueue -> SomeException -> [(String, String)] -> io ()
queueErrorReport q message = liftIO . writeAsyncQueue q . ErrorReport message

-- | Queue a stat to be sent to statsd.
queueStat :: MonadIO io => StatQueue -> Stat -> io ()
queueStat q = liftIO . writeAsyncQueue q


-- Eff interface

-- | A task which logs a message at a specific log level to stderr.
writeLog :: Has Telemetry sig m => Level -> String -> [(String, String)] -> m ()
writeLog level message pairs = send (WriteLog level message pairs)

-- | A task which writes a stat.
writeStat :: Has Telemetry sig m => Stat -> m ()
writeStat stat = send (WriteStat stat)

-- | A task which measures and stats the timing of another task.
time :: (Has Telemetry sig m, MonadIO m) => String -> [(String, String)] -> m output -> m output
time statName tags task = do
  (a, stat) <- withTiming statName tags task
  a <$ writeStat stat

-- | A task which measures and returns the timing of another task.
time' :: MonadIO m => m output -> m (output, Double)
time' = withTiming'

-- | Statting and logging effects.
data Telemetry (m :: * -> *) k where
  WriteStat :: Stat -> Telemetry m ()
  WriteLog :: Level -> String -> [(String, String)] -> Telemetry m ()

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: LogQueue -> StatQueue -> TelemetryC m a -> m a
runTelemetry logger statter = runReader (logger, statter) . runTelemetryC

newtype TelemetryC m a = TelemetryC { runTelemetryC :: ReaderC (LogQueue, StatQueue) m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Telemetry :+: sig) (TelemetryC m) where
  alg hdl sig ctx = case sig of
    L op -> do
      queues <- TelemetryC (ask @(LogQueue, StatQueue))
      case op of
        WriteStat stat               -> ctx <$ queueStat (snd queues) stat
        WriteLog level message pairs -> ctx <$ queueLogMessage (fst queues) level message pairs
    R other -> TelemetryC (alg (runTelemetryC . hdl) (R other) ctx)

-- | Run a 'Telemetry' effect by ignoring statting/logging.
ignoreTelemetry :: IgnoreTelemetryC m a -> m a
ignoreTelemetry = runIgnoreTelemetryC

newtype IgnoreTelemetryC m a = IgnoreTelemetryC { runIgnoreTelemetryC :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Telemetry :+: sig) (IgnoreTelemetryC m) where
  alg hdl sig ctx = case sig of
    L WriteStat{} -> pure ctx
    L WriteLog{}  -> pure ctx
    R other       -> IgnoreTelemetryC (alg (runIgnoreTelemetryC . hdl) other ctx)
