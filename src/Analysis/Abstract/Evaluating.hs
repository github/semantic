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
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Semigroup
import Data.Set hiding (foldr)

type Interpreter v = '[Fail, State (Store (LocationFor v) v), Reader (Set (Address (LocationFor v) v)), Reader (Environment (LocationFor v) v)]

type MonadInterpreter v m = (MonadEnv v m, MonadStore v m, MonadFail m)

type EvalResult v = Final (Interpreter v) v

type Eval' t m v = (v -> m v) -> t -> m v

-- Evaluate an expression.
-- Example:
--    evaluate @Type <term>
--    evaluate @(Value (Data.Union.Union Language.Python.Assignment2.Syntax) (Record Location) Precise) <term>
evaluate :: forall v term
         . ( Ord v
           , Semigroup (Cell (LocationFor v) v)
           , Functor (Base term)
           , Recursive term
           , MonadAddress (LocationFor v) (Eff (Interpreter v))
           , Eval term v (Eff (Interpreter v)) (Base term)
           )
         => term
         -> EvalResult v
evaluate = run @(Interpreter v) . fix ev pure

ev :: ( Functor (Base term)
      , Recursive term
      , Eval term v m (Base term)
      )
      => Eval' term m v -> Eval' term m v
ev recur yield = eval recur yield . project
