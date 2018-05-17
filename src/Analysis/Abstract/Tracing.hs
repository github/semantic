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
                , Members '[ Reader (Live location value)
                           , State (Environment location value)
                           , State (Heap location (Cell location) value)
                           , Writer (trace (Configuration term location (Cell location) value))
                           ] effects
                , Reducer (Configuration term location (Cell location) value) (trace (Configuration term location (Cell location) value))
                )
             => trace (Configuration term location (Cell location) value)
             -> SubtermAlgebra (Base term) term (TermEvaluator term location value effects a)
             -> SubtermAlgebra (Base term) term (TermEvaluator term location value effects a)
tracingTerms proxy recur term = getConfiguration (embedSubterm term) >>= trace . (`asTypeOf` proxy) . Reducer.unit >> recur term

trace :: Member (Writer (trace (Configuration term location (Cell location) value))) effects => trace (Configuration term location (Cell location) value) -> TermEvaluator term location value effects ()
trace = tell

tracing :: Monoid (trace (Configuration term location (Cell location) value)) => TermEvaluator term location value (Writer (trace (Configuration term location (Cell location) value)) ': effects) a -> TermEvaluator term location value effects (a, trace (Configuration term location (Cell location) value))
tracing = runWriter
