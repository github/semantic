{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Abstract.Interpreter
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Env
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Store
import Control.Monad.Effect.Trace
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Eval
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Classes (Ord1)
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import GHC.Exts (IsList(..))

type TracingInterpreter t v g = Reader (Set.Set (Address (LocationFor v) v)) ': Writer (g (Configuration (LocationFor v) t v)) ': Interpreter v

type TraceInterpreter t v = TracingInterpreter t v []
type ReachableStateInterpreter t v = TracingInterpreter t v Set.Set


-- Tracing and reachable state analyses
--
-- Examples
--    evalTrace @(Value Syntax Precise) <term>
--    evalReach @(Value Syntax Precise) <term>

evalTrace :: forall v syntax ann
          . ( Ord v, Ord ann, Ord1 syntax, Ord (Cell (LocationFor v) v)
            , FreeVariables1 syntax
            , Functor syntax
            , MonadAddress (LocationFor v) (Eff (TraceInterpreter (Term syntax ann) v))
            , MonadGC v (Eff (TraceInterpreter (Term syntax ann) v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval (Term syntax ann) v (Eff (TraceInterpreter (Term syntax ann) v)) syntax
            )
          => Term syntax ann -> Final (TracingInterpreter (Term syntax ann) v []) v
evalTrace = run @(TraceInterpreter (Term syntax ann) v) . fix (evTell @[] ev) pure

evalReach :: forall v syntax ann
          . ( Ord v, Ord ann, Ord (LocationFor v), Ord (Cell (LocationFor v) v), Ord1 syntax
            , FreeVariables1 syntax
            , Functor syntax
            , MonadAddress (LocationFor v) (Eff (ReachableStateInterpreter (Term syntax ann) v))
            , MonadGC v (Eff (ReachableStateInterpreter (Term syntax ann) v))
            , Semigroup (Cell (LocationFor v) v)
            , Eval (Term syntax ann) v (Eff (ReachableStateInterpreter (Term syntax ann) v)) syntax
            )
          => Term syntax ann -> Final (TracingInterpreter (Term syntax ann) v Set.Set) v
evalReach = run @(ReachableStateInterpreter (Term syntax ann) v) . fix (evTell @Set.Set ev) pure


evTell :: forall g t m v
       . ( IsList (g (Configuration (LocationFor v) t v))
         , Item (g (Configuration (LocationFor v) t v)) ~ Configuration (LocationFor v) t v
         , MonadTrace t v g m
         , MonadEnv v m
         , MonadStore v m
         , MonadGC v m
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
