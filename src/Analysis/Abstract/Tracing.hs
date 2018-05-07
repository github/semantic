{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Tracing
( tracingTerms
, tracing
) where

import Control.Abstract
import Control.Monad.Effect.Writer
import Data.Semigroup.Reducer as Reducer
import Prologue hiding (trace)

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
tracingTerms :: ( Corecursive term
                , Members '[ Reader (Live location value)
                           , State (Environment location value)
                           , State (Heap location value)
                           , Writer (trace (Configuration location term value))
                           ] effects
                , Reducer (Configuration location term value) (trace (Configuration location term value))
                )
             => trace (Configuration location term value)
             -> SubtermAlgebra (Base term) term (Evaluator location term value effects a)
             -> SubtermAlgebra (Base term) term (Evaluator location term value effects a)
tracingTerms proxy recur term = getConfiguration (embedSubterm term) >>= trace . (`asTypeOf` proxy) . Reducer.unit >> recur term

trace :: Member (Writer (trace (Configuration location term value))) effects => trace (Configuration location term value) -> Evaluator location term value effects ()
trace = raise . tell

tracing :: Monoid (trace (Configuration location term value)) => Evaluator location term value (Writer (trace (Configuration location term value)) ': effects) a -> Evaluator location term value effects (a, trace (Configuration location term value))
tracing = raiseHandler runWriter
