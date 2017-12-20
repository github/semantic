{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications #-}
module Analysis.Abstract.Evaluating where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Store
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.Live
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Semigroup
import Data.Set

type Interpreter v
  = '[ Fail
     , Reader (Live (LocationFor v) v)
     , State (Store (LocationFor v) v)
     , Reader (Set (Address (LocationFor v) v))
     , Reader (Environment (LocationFor v) v)
     ]

type MonadInterpreter v m = (MonadEnv v m, MonadStore v m, MonadFail m)

type EvalResult v = Final (Interpreter v) v

-- | Evaluate a term to a value.
evaluate :: forall v term
         . ( Ord v
           , Ord (Cell (LocationFor v) v)
           , Semigroup (Cell (LocationFor v) v)
           , Functor (Base term)
           , Recursive term
           , MonadAddress (LocationFor v) (Eff (Interpreter v))
           , Eval term v (Eff (Interpreter v)) (Base term)
           )
         => term
         -> EvalResult v
evaluate = run @(Interpreter v) . fix (\ recur yield -> eval recur yield . project) pure
