{-# LANGUAGE FlexibleContexts #-}
module Analysis.Abstract.Tracing
( tracingTerms
, tracing
) where

import Control.Abstract hiding (trace)
import Control.Effect.Writer
import Data.Semigroup.Reducer as Reducer

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
tracingTerms :: ( Member (State (Heap address address value)) sig
               , Member (Writer (trace (Configuration term address value))) sig
               , Carrier sig m
               , Reducer (Configuration term address value) (trace (Configuration term address value))
               )
             => trace (Configuration term address value)
             -> Open (term -> Evaluator term address value m a)
tracingTerms proxy recur term = getConfiguration term >>= trace . (`asTypeOf` proxy) . Reducer.unit >> recur term

trace :: ( Member (Writer (trace (Configuration term address value))) sig
        , Carrier sig m
        )
      => trace (Configuration term address value)
      -> Evaluator term address value m ()
trace = tell

tracing :: (Monoid (trace (Configuration term address value)))
        => Evaluator term address value (WriterC (trace (Configuration term address value)) (Evaluator term address value m)) a
        -> Evaluator term address value m (trace (Configuration term address value), a)
tracing = runWriter . runEvaluator


-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (State (Heap address address value)) sig, Carrier sig m)
                 => term
                 -> Evaluator term address value m (Configuration term address value)
getConfiguration term = Configuration term <$>  getHeap

-- | A single point in a program’s execution.
data Configuration term address value = Configuration
  { configurationTerm :: term                       -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationHeap :: Heap address address value -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)
