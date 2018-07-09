{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Tracing
( tracingTerms
, tracing
) where

import Control.Abstract hiding (trace)
import Control.Monad.Effect.Writer
import Data.Semigroup.Reducer as Reducer
import Prologue

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
tracingTerms :: ( Corecursive term
                , Member (Reader (Live address)) effects
                , Member (Env address) effects
                , Member (State (Heap address (Cell address) value)) effects
                , Member (Writer (trace (Configuration term address (Cell address) value))) effects
                , Reducer (Configuration term address (Cell address) value) (trace (Configuration term address (Cell address) value))
                )
             => trace (Configuration term address (Cell address) value)
             -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
             -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
tracingTerms proxy recur term = getConfiguration (embedSubterm term) >>= trace . (`asTypeOf` proxy) . Reducer.unit >> recur term

trace :: Member (Writer (trace (Configuration term address (Cell address) value))) effects => trace (Configuration term address (Cell address) value) -> TermEvaluator term address value effects ()
trace = tell

tracing :: (Monoid (trace (Configuration term address (Cell address) value)), Effects effects) => TermEvaluator term address value (Writer (trace (Configuration term address (Cell address) value)) ': effects) a -> TermEvaluator term address value effects (trace (Configuration term address (Cell address) value), a)
tracing = runWriter
