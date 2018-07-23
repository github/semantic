{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Telemetry
(
  -- Async telemetry interface
  withLogger
, withHaystack
, withStatter
, LogQueue
, StatQueue
, HaystackQueue
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

-- Haystack client
, haystackClient
, HaystackClient

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
, Telemetry
, runTelemetry
, ignoreTelemetry
) where

import           Control.Exception
import           Control.Monad.Effect
import           Control.Monad.IO.Class
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.LocalTime as LocalTime
import           Network.HTTP.Client
import           Semantic.Telemetry.AsyncQueue
import           Semantic.Telemetry.Haystack
import           Semantic.Telemetry.Log
import           Semantic.Telemetry.Stat as Stat

type LogQueue = AsyncQueue Message LogOptions
type StatQueue = AsyncQueue Stat StatsClient
type HaystackQueue = AsyncQueue ErrorReport HaystackClient

data TelemetryQueues
  = TelemetryQueues
  { telemetryLogger   :: LogQueue
  , telemetryStatter  :: StatQueue
  , telemetryHaystack :: HaystackQueue
  }

-- | Execute an action in IO with access to a logger (async log queue).
withLogger :: LogOptions         -- ^ Log options
           -> Int                -- ^ Max stats queue size before dropping stats
           -> (LogQueue -> IO c) -- ^ Action in IO
           -> IO c
withLogger options size = bracket setup closeAsyncQueue
  where setup = newAsyncQueue size writeLogMessage options

-- | Execute an action in IO with access to haystack (async error reporting queue).
withHaystack :: Maybe String -> ManagerSettings -> String -> ErrorLogger -> Int -> (HaystackQueue -> IO c) -> IO c
withHaystack url settings appName errorLogger size = bracket setup closeAsyncQueue
  where setup = haystackClient url settings appName >>= newAsyncQueue size (reportError errorLogger)

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

-- | Queue an error to be reported to haystack.
queueErrorReport :: MonadIO io => HaystackQueue -> SomeException -> [(String, String)] -> io ()
queueErrorReport q@AsyncQueue{..} message = liftIO . writeAsyncQueue q . ErrorReport message

-- | Queue a stat to be sent to statsd.
queueStat :: MonadIO io => StatQueue -> Stat -> io ()
queueStat q = liftIO . writeAsyncQueue q


-- Eff interface

-- | A task which logs a message at a specific log level to stderr.
writeLog :: Member Telemetry effs => Level -> String -> [(String, String)] -> Eff effs ()
writeLog level message pairs = send (WriteLog level message pairs)

-- | A task which writes a stat.
writeStat :: Member Telemetry effs => Stat -> Eff effs ()
writeStat stat = send (WriteStat stat)

-- | A task which measures and stats the timing of another task.
time :: (Member (Lift IO) effs, Member Telemetry effs) => String -> [(String, String)] -> Eff effs output -> Eff effs output
time statName tags task = do
  (a, stat) <- withTiming statName tags task
  a <$ writeStat stat


-- | Statting and logging effects.
data Telemetry (m :: * -> *) output where
  WriteStat :: Stat                                  -> Telemetry m ()
  WriteLog  :: Level -> String -> [(String, String)] -> Telemetry m ()

instance PureEffect Telemetry
instance Effect Telemetry where
  handleState c dist (Request (WriteStat stat) k) = Request (WriteStat stat) (dist . (<$ c) . k)
  handleState c dist (Request (WriteLog level message pairs) k) = Request (WriteLog level message pairs) (dist . (<$ c) . k)

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: (Member (Lift IO) effects, Effects effects) => LogQueue -> StatQueue -> Eff (Telemetry ': effects) a -> Eff effects a
runTelemetry logger statter = interpret (\ t -> case t of
  WriteStat stat -> queueStat statter stat
  WriteLog level message pairs -> queueLogMessage logger level message pairs)

-- | Run a 'Telemetry' effect by ignoring statting/logging.
ignoreTelemetry :: Effects effs => Eff (Telemetry ': effs) a -> Eff effs a
ignoreTelemetry = interpret (\ t -> case t of
  WriteStat{} -> pure ()
  WriteLog{}  -> pure ())
