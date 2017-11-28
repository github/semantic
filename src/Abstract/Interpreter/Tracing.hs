{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Tracing where

import Abstract.Configuration
import Abstract.Environment
import Abstract.Eval
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Data.Term

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Function (fix)
import Data.Functor.Classes (Ord1)
import Data.Semigroup
import GHC.Exts (IsList(..))
import qualified Data.Set as Set

type TracingInterpreter l t v g = Reader (Set (Address l v)) ': Writer (g (Configuration l t v)) ': Interpreter l v

type TraceInterpreter l t v = TracingInterpreter l t v []
type ReachableStateInterpreter l t v = TracingInterpreter l t v Set.Set


class Monad m => MonadTrace l t v g m where
  trace :: g (Configuration l t v) -> m ()

instance (Writer (g (Configuration l t v)) :< fs) => MonadTrace l t v g (Eff fs) where
  trace = tell

-- Tracing and reachable state analyses
--
-- Examples
--    evalTrace @Precise @(Value Syntax Precise) @Syntax (makeLam "x" (var "x") # true)
--    evalReach @Precise @(Value Syntax Precise) @Syntax (makeLam "x" (var "x") # true)

evalTrace :: forall l v syntax ann
          . ( Ord v, Ord ann, Ord1 syntax, Ord1 (Cell l)
            , MonadAddress l (Eff (TraceInterpreter l (Term syntax ann) v))
            , MonadPrim v (Eff (TraceInterpreter l (Term syntax ann) v))
            , MonadGC l v (Eff (TraceInterpreter l (Term syntax ann) v))
            , Semigroup (Cell l v)
            , Eval l v (Eff (TraceInterpreter l (Term syntax ann) v)) syntax ann syntax
            )
          => Term syntax ann -> Final (TracingInterpreter l (Term syntax ann) v []) v
evalTrace = run @(TraceInterpreter l (Term syntax ann) v) . fix (evTell @l @(Term syntax ann) @v @[] (ev @l)) pure

evalReach :: forall l v syntax ann
          . ( Ord v, Ord ann, Ord l, Ord1 (Cell l), Ord1 syntax
            , MonadAddress l (Eff (ReachableStateInterpreter l (Term syntax ann) v))
            , MonadPrim v (Eff (ReachableStateInterpreter l (Term syntax ann) v))
            , MonadGC l v (Eff (ReachableStateInterpreter l (Term syntax ann) v))
            , Semigroup (Cell l v)
            , Eval l v (Eff (ReachableStateInterpreter l (Term syntax ann) v)) syntax ann syntax
            )
          => Term syntax ann -> Final (TracingInterpreter l (Term syntax ann) v Set.Set) v
evalReach = run @(ReachableStateInterpreter l (Term syntax ann) v) . fix (evTell @l @(Term syntax ann) @v @Set.Set (ev @l)) pure


evTell :: forall l t v g m
       . ( Ord l
         , IsList (g (Configuration l t v))
         , Item (g (Configuration l t v)) ~ Configuration l t v
         , MonadTrace l t v g m
         , MonadEnv l v m
         , MonadStore l v m
         , MonadGC l v m
         )
       => (Eval' t m v -> Eval' t m v)
       -> Eval' t m v
       -> Eval' t m v
evTell ev0 ev yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  trace (fromList [Configuration e roots env store] :: g (Configuration l t v))
  ev0 ev yield e
