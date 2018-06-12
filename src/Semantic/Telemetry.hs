{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Telemetry
( writeLog
, writeStat
, time
, Telemetry
, runTelemetry
, ignoreTelemetry
) where

import Control.Monad.Effect
import Control.Monad.IO.Class
import Semantic.Log
import Semantic.Queue
import Semantic.Stat

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

instance Effect Telemetry where
  handleState c dist (Request (WriteStat stat) k) = Request (WriteStat stat) (dist . (<$ c) . k)
  handleState c dist (Request (WriteLog level message pairs) k) = Request (WriteLog level message pairs) (dist . (<$ c) . k)

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: (Member (Lift IO) effects, Effects effects) => LogQueue -> AsyncQueue Stat StatsClient -> Eff (Telemetry ': effects) a -> Eff effects a
runTelemetry logger statter = interpret (\ t -> case t of
  WriteStat stat -> liftIO (queue statter stat)
  WriteLog level message pairs -> queueLogMessage logger level message pairs)

-- | Run a 'Telemetry' effect by ignoring statting/logging.
ignoreTelemetry :: Effects effs => Eff (Telemetry ': effs) a -> Eff effs a
ignoreTelemetry = interpret (\ t -> case t of
  WriteStat{} -> pure ()
  WriteLog{}  -> pure ())
