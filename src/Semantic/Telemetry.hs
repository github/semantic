{-# LANGUAGE GADTs, TypeOperators #-}
module Semantic.Telemetry where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.IO.Class
import Semantic.Log
import Semantic.Queue
import Semantic.Stat

-- | Statting and logging effects.
data Telemetry output where
  WriteStat :: Stat                                  -> Telemetry ()
  WriteLog  :: Level -> String -> [(String, String)] -> Telemetry ()

-- | A task which logs a message at a specific log level to stderr.
writeLog :: Member Telemetry effs => Level -> String -> [(String, String)] -> Eff effs ()
writeLog level message pairs = send (WriteLog level message pairs)

-- | A task which writes a stat.
writeStat :: Member Telemetry effs => Stat -> Eff effs ()
writeStat stat = send (WriteStat stat)


-- | A queue for logging.
type LogQueue = AsyncQueue Message Options

-- | A queue for stats.
type StatQueue = AsyncQueue Stat StatsClient


runTelemetry :: Members '[Reader LogQueue, Reader StatQueue, IO] effs => Eff (Telemetry ': effs) a -> Eff effs a
runTelemetry = interpret (\ t -> case t of
  WriteStat stat -> ask >>= \ statter -> liftIO (queue (statter :: StatQueue) stat)
  WriteLog level message pairs -> ask >>= \ logger -> queueLogMessage logger level message pairs)
