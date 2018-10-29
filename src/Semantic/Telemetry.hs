{-# LANGUAGE GADTs, KindSignatures, LambdaCase, RankNTypes, TypeOperators, UndecidableInstances #-}
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
, time'
, Telemetry(..)
, runTelemetry
, TelemetryC(..)
, ignoreTelemetry
, IgnoreTelemetryC(..)
) where

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Coerce
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
writeLog :: (Member Telemetry sig, Carrier sig m) => Level -> String -> [(String, String)] -> m ()
writeLog level message pairs = send (WriteLog level message pairs (ret ()))

-- | A task which writes a stat.
writeStat :: (Member Telemetry sig, Carrier sig m) => Stat -> m ()
writeStat stat = send (WriteStat stat (ret ()))

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
  = WriteStat Stat k
  | WriteLog Level String [(String, String)] k
  deriving (Functor)

instance HFunctor Telemetry where
  hmap _ = coerce

instance Effect Telemetry where
  handle state handler (WriteStat stat k) = WriteStat stat (handler (k <$ state))
  handle state handler (WriteLog level message pairs k) = WriteLog level message pairs (handler (k <$ state))

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: (Carrier sig m, MonadIO m) => LogQueue -> StatQueue -> Eff (TelemetryC m) a -> m a
runTelemetry logger statter = flip runTelemetryC (logger, statter) . interpret

newtype TelemetryC m a = TelemetryC { runTelemetryC :: (LogQueue, StatQueue) -> m a }

instance (Carrier sig m, MonadIO m) => Carrier (Telemetry :+: sig) (TelemetryC m) where
  ret = TelemetryC . const . ret
  eff op = TelemetryC (\ queues -> handleSum (eff . handleReader queues runTelemetryC) (\case
    WriteStat stat               k -> queueStat (snd queues) stat *> runTelemetryC k queues
    WriteLog level message pairs k -> queueLogMessage (fst queues) level message pairs *> runTelemetryC k queues) op)


-- | Run a 'Telemetry' effect by ignoring statting/logging.
ignoreTelemetry :: Carrier sig m => Eff (IgnoreTelemetryC m) a -> m a
ignoreTelemetry = runIgnoreTelemetryC . interpret

newtype IgnoreTelemetryC m a = IgnoreTelemetryC { runIgnoreTelemetryC :: m a }

instance Carrier sig m => Carrier (Telemetry :+: sig) (IgnoreTelemetryC m) where
  ret = IgnoreTelemetryC . ret
  eff = handleSum (IgnoreTelemetryC . eff . handlePure runIgnoreTelemetryC) (\case
    WriteStat _    k -> k
    WriteLog _ _ _ k -> k)
