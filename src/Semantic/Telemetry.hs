{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Telemetry
(
  LogQueue
, StatQueue
, HaystackQueue
, queueLogMessage
, queueErrorReport
, queueStat

, Stat.count
, statsClient
, StatsClient

, haystackClient
, HaystackClient

, Level(..)
, LogOptions(..)
, logfmtFormatter
, terminalFormatter
, LogFormatter

-- Eff interface
, writeLog
, writeStat
, time
, Telemetry
, runTelemetry
, ignoreTelemetry
) where

import Control.Exception
import Control.Monad.Effect
import Control.Monad.IO.Class
import Semantic.Telemetry.AsyncQueue
import Semantic.Telemetry.Haystack
import Semantic.Telemetry.Log
import Semantic.Telemetry.Stat as Stat
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as LocalTime

type LogQueue = AsyncQueue Message LogOptions
type StatQueue = AsyncQueue Stat StatsClient
type HaystackQueue = AsyncQueue ErrorReport HaystackClient

-- | Queue a message to be logged.
queueLogMessage :: MonadIO io => LogQueue -> Level -> String -> [(String, String)] -> io ()
queueLogMessage q@AsyncQueue{..} level message pairs
  | Just logLevel <- optionsLevel asyncQueueExtra
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
time :: (Member IO effs, Member Telemetry effs) => String -> [(String, String)] -> Eff effs output -> Eff effs output
time statName tags task = do
  (a, stat) <- withTiming statName tags task
  a <$ writeStat stat


-- | Statting and logging effects.
data Telemetry output where
  WriteStat :: Stat                                  -> Telemetry ()
  WriteLog  :: Level -> String -> [(String, String)] -> Telemetry ()

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: Member IO effects => LogQueue -> StatQueue -> Eff (Telemetry ': effects) a -> Eff effects a
runTelemetry logger statter = interpret (\ t -> case t of
  WriteStat stat -> queueStat statter stat
  WriteLog level message pairs -> queueLogMessage logger level message pairs)

-- | Run a 'Telemetry' effect by ignoring statting/logging.
ignoreTelemetry :: Eff (Telemetry ': effs) a -> Eff effs a
ignoreTelemetry = interpret (\ t -> case t of
  WriteStat{} -> pure ()
  WriteLog{}  -> pure ())
