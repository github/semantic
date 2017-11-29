{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Tracing where

import Abstract.Configuration
import Abstract.Environment
import Abstract.Eval
import Abstract.FreeVariables
import Abstract.Interpreter
import Abstract.Store
import Abstract.Value

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Function (fix)
import Data.Functor.Classes (Ord1)
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import GHC.Exts (IsList(..))

type TracingInterpreter l t v g = Reader (Set.Set (Address l v)) ': Writer (g (Configuration l t v)) ': Interpreter l v

type TraceInterpreter l t v = TracingInterpreter l t v []
type ReachableStateInterpreter l t v = TracingInterpreter l t v Set.Set


class Monad m => MonadTrace l t v g m where
  trace :: g (Configuration l t v) -> m ()

instance (Writer (g (Configuration l t v)) :< fs) => MonadTrace l t v g (Eff fs) where
  trace = tell

-- Tracing and reachable state analyses
--
-- Examples
--    evalTrace @(Value Syntax Precise) <term>
--    evalReach @(Value Syntax Precise) <term>

evalTrace :: forall v syntax ann
          . ( Ord v, Ord ann, Ord1 syntax, Ord1 (Cell (LocationFor v))
            , FreeVariables1 syntax
            , Functor syntax
            , MonadAddress (LocationFor v) (Eff (TraceInterpreter (LocationFor v) (Term syntax ann) v))
            , MonadGC (LocationFor v) v (Eff (TraceInterpreter (LocationFor v) (Term syntax ann) v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval v (Eff (TraceInterpreter (LocationFor v) (Term syntax ann) v)) syntax
            )
          => Term syntax ann -> Final (TracingInterpreter (LocationFor v) (Term syntax ann) v []) v
evalTrace = run @(TraceInterpreter (LocationFor v) (Term syntax ann) v) . fix (evTell @[] ev) pure

evalReach :: forall v syntax ann
          . ( Ord v, Ord ann, Ord (LocationFor v), Ord1 (Cell (LocationFor v)), Ord1 syntax
            , FreeVariables1 syntax
            , Functor syntax
            , MonadAddress (LocationFor v) (Eff (ReachableStateInterpreter (LocationFor v) (Term syntax ann) v))
            , MonadGC (LocationFor v) v (Eff (ReachableStateInterpreter (LocationFor v) (Term syntax ann) v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval v (Eff (ReachableStateInterpreter (LocationFor v) (Term syntax ann) v)) syntax
            )
          => Term syntax ann -> Final (TracingInterpreter (LocationFor v) (Term syntax ann) v Set.Set) v
evalReach = run @(ReachableStateInterpreter (LocationFor v) (Term syntax ann) v) . fix (evTell @Set.Set ev) pure


evTell :: forall g t m v
       . ( IsList (g (Configuration (LocationFor v) t v))
         , Item (g (Configuration (LocationFor v) t v)) ~ Configuration (LocationFor v) t v
         , MonadTrace (LocationFor v) t v g m
         , MonadEnv (LocationFor v) v m
         , MonadStore (LocationFor v) v m
         , MonadGC (LocationFor v) v m
         )
       => (Eval' t m v -> Eval' t m v)
       -> Eval' t m v
       -> Eval' t m v
evTell ev0 ev' yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  trace (fromList [Configuration e (Set.toList roots) env store] :: g (Configuration (LocationFor v) t v))
  ev0 ev' yield e
