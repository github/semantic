{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, MonoLocalBinds #-}
module Abstract.Interpreter where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Store
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.FreeVariables
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Function (fix)
import Data.Semigroup
import Data.Set hiding (foldr)
import Data.Term
import Prelude hiding (fail)


type Interpreter v = '[Fresh, Fail, NonDetEff, State (Store (LocationFor v) v), Reader (Set (Address (LocationFor v) v)), Reader (Environment (LocationFor v) v)]

type MonadInterpreter v m = (MonadEnv v m, MonadStore v m, MonadFail m)

type EvalResult v = Final (Interpreter v) v

type Eval' t m v = (v -> m v) -> t -> m v

-- Evaluate an expression.
-- Example:
--    evaluate @Type <term>
--    evaluate @(Value (Data.Union.Union Language.Python.Assignment2.Syntax) (Record Location) Precise) <term>
evaluate :: forall v syntax ann
         . ( Ord v
           , Functor syntax
           , Semigroup (Cell (LocationFor v) v)
           , FreeVariables1 syntax
           , MonadAddress (LocationFor v) (Eff (Interpreter v))
           , Eval (Term syntax ann) v (Eff (Interpreter v)) syntax
           )
         => Term syntax ann
         -> EvalResult v
evaluate = run @(Interpreter v) . fix ev pure

ev ::
     ( Functor syntax
     , FreeVariables1 syntax
     , Eval (Term syntax ann) v m syntax
     )
     => Eval' (Term syntax ann) m v -> Eval' (Term syntax ann) m v
ev recur yield = eval recur yield . unTerm
