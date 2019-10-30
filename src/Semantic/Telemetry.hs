{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, RecordWildCards, TypeOperators, UndecidableInstances #-}
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

import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.LocalTime as LocalTime
import           GHC.Generics (Generic1)
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
queueErrorReport q@AsyncQueue{..} message = liftIO . writeAsyncQueue q . ErrorReport message

-- | Queue a stat to be sent to statsd.
queueStat :: MonadIO io => StatQueue -> Stat -> io ()
queueStat q = liftIO . writeAsyncQueue q


-- Eff interface

-- | A task which logs a message at a specific log level to stderr.
writeLog :: (Member Telemetry sig, Carrier sig m) => Level -> String -> [(String, String)] -> m ()
writeLog level message pairs = send (WriteLog level message pairs (pure ()))

-- | A task which writes a stat.
writeStat :: (Member Telemetry sig, Carrier sig m) => Stat -> m ()
writeStat stat = send (WriteStat stat (pure ()))

-- | A task which measures and stats the timing of another task.
time :: (Member Telemetry sig, Carrier sig m, MonadIO m) => String -> [(String, String)] -> m output -> m output
time statName tags task = do
  (a, stat) <- withTiming statName tags task
  a <$ writeStat stat

-- | A task which measures and returns the timing of another task.
time' :: MonadIO m => m output -> m (output, Double)
time' = withTiming'

-- | Statting and logging effects.
data Telemetry (m :: * -> *) k
  = WriteStat Stat (m k)
  | WriteLog Level String [(String, String)] (m k)
  deriving (Functor, Generic1)

instance HFunctor Telemetry
instance Effect   Telemetry

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: LogQueue -> StatQueue -> TelemetryC m a -> m a
runTelemetry logger statter = runReader (logger, statter) . runTelemetryC

newtype TelemetryC m a = TelemetryC { runTelemetryC :: ReaderC (LogQueue, StatQueue) m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Carrier sig m, MonadIO m) => Carrier (Telemetry :+: sig) (TelemetryC m) where
  eff (L op) = do
    queues <- TelemetryC ask
    case op of
      WriteStat stat k               -> queueStat (snd queues) stat *> k
      WriteLog level message pairs k -> queueLogMessage (fst queues) level message pairs *> k
  eff (R other) = TelemetryC (eff (R (handleCoercible other)))

-- | Run a 'Telemetry' effect by ignoring statting/logging.
ignoreTelemetry :: IgnoreTelemetryC m a -> m a
ignoreTelemetry = runIgnoreTelemetryC

newtype IgnoreTelemetryC m a = IgnoreTelemetryC { runIgnoreTelemetryC :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig m => Carrier (Telemetry :+: sig) (IgnoreTelemetryC m) where
  eff (R other) = IgnoreTelemetryC . eff . handleCoercible $ other
  eff (L (WriteStat _ k))    = k
  eff (L (WriteLog _ _ _ k)) = k
