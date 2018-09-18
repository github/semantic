{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Tracing
( tracingTerms
, tracing
) where

import Control.Abstract hiding (trace, Configuration)
import Control.Monad.Effect.Writer
import Data.Abstract.Environment
import Data.Semigroup.Reducer as Reducer
import Prologue

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
tracingTerms :: ( Corecursive term
                , Member (Env address) effects
                , Member (State (Heap address value)) effects
                , Member (Writer (trace (Configuration term address value))) effects
                , Reducer (Configuration term address value) (trace (Configuration term address value))
                )
             => trace (Configuration term address value)
             -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
             -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
tracingTerms proxy recur term = getConfiguration (embedSubterm term) >>= trace . (`asTypeOf` proxy) . Reducer.unit >> recur term

trace :: Member (Writer (trace (Configuration term address value))) effects => trace (Configuration term address value) -> TermEvaluator term address value effects ()
trace = tell

tracing :: (Monoid (trace (Configuration term address value)), Effects effects) => TermEvaluator term address value (Writer (trace (Configuration term address value)) ': effects) a -> TermEvaluator term address value effects (trace (Configuration term address value), a)
tracing = runWriter


-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Env address) effects, Member (State (Heap address value)) effects)
                 => term
                 -> TermEvaluator term address value effects (Configuration term address value)
getConfiguration term = Configuration term <$> TermEvaluator getEvalContext <*> TermEvaluator getHeap

-- | A single point in a program’s execution.
data Configuration term address value = Configuration
  { configurationTerm    :: term                -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationContext :: EvalContext address -- ^ The evaluation context in 'configurationTerm'.
  , configurationHeap    :: Heap address value  -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)
