{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter where

import Abstract.Environment
import Abstract.Primitive
import Abstract.Store
import Abstract.Type
import Abstract.Eval
import Data.Term

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Semigroup
import Prelude hiding (fail)


type Interpreter l v = '[Fresh, Fail, NonDetEff, State (Store l v), Reader (Environment l v)]

type MonadInterpreter l v m = (MonadEnv l v m, MonadStore l v m, MonadFail m)

type EvalResult l v = Final (Interpreter l v) v

type Eval' t m = t -> m

-- Evaluate an expression.
-- Example:
--    evaluate @Precise @(Value Syntax Precise) @Syntax (makeLam "x" (var "x") # true)
--    Files.readFile "test.py" (Just Python) >>= runTask . parse pythonParser2 >>= pure . evaluate @Precise @(Value (Data.Union.Union Language.Python.Assignment2.Syntax) (Record Location) Precise) @(Data.Union.Union Language.Python.Assignment2.Syntax) @(Record Location)
evaluate :: forall l v syntax ann
         . ( Ord v
           , Eval v (Eff (Interpreter l v)) syntax ann syntax
           , MonadAddress l (Eff (Interpreter l v))
           , MonadPrim v (Eff (Interpreter l v))
           , Semigroup (Cell l v))
         => Term syntax ann
         -> EvalResult l v
evaluate = run @(Interpreter l v) . fix ev

ev :: (Eval v m syntax ann syntax)
   => (Term syntax ann -> m v)
   -> Term syntax ann -> m v
ev ev' = eval ev' . unTerm
