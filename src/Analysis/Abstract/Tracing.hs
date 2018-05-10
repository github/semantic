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
                           , State (Heap (Cell location) location value)
                           , Writer (trace (Configuration term (Cell location) location value))
                           ] effects
                , Reducer (Configuration term (Cell location) location value) (trace (Configuration term (Cell location) location value))
                )
             => trace (Configuration term (Cell location) location value)
             -> SubtermAlgebra (Base term) term (Evaluator location value effects a)
             -> SubtermAlgebra (Base term) term (Evaluator location value effects a)
tracingTerms proxy recur term = getConfiguration (embedSubterm term) >>= trace . (`asTypeOf` proxy) . Reducer.unit >> recur term

trace :: Member (Writer (trace (Configuration term (Cell location) location value))) effects => trace (Configuration term (Cell location) location value) -> Evaluator location value effects ()
trace = tell

tracing :: Monoid (trace (Configuration term (Cell location) location value)) => Evaluator location value (Writer (trace (Configuration term (Cell location) location value)) ': effects) a -> Evaluator location value effects (a, trace (Configuration term (Cell location) location value))
tracing = runWriter
