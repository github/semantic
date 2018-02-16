{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Eval2
( Eval(..)
, MonadGC(..)
, MonadFail(..)
, Recursive(..)
, Base
) where

import Control.Monad.Effect.Env
import Control.Monad.Effect.GC
import Control.Monad.Fail
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Data.Functor.Classes
import Data.Proxy
import Data.Term
import Data.Union
import Data.Functor.Foldable (Base, Recursive(..), project)
import Prelude hiding (fail)

-- | The 'Eval' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Monad m => Eval term v m constr where
  eval :: constr term -> m v

  default eval :: (MonadFail m, Show1 constr) => (constr term -> m v)
  eval expr = fail $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance (Monad m, Apply (Eval t v m) fs) => Eval t v m (Union fs) where
  eval = apply (Proxy :: Proxy (Eval t v m)) eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance (Monad m, Eval t v m s) => Eval t v m (TermF s a) where
  eval In{..} = eval termFOut

-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by yielding under 'localEnv'); and
--   3. Only the last statement’s return value is returned.
--
--   This also allows e.g. early returns to be implemented in the middle of a list, by means of a statement returning instead of yielding. Therefore, care must be taken by 'Eval' instances in general to yield and not simply return, or else they will unintentionally short-circuit control and skip the rest of the scope.
instance ( Monad m
         , Ord (LocationFor v)
         -- , MonadGC v m
         -- , MonadEnv v m
         , AbstractValue v
         , Recursive t
         -- , FreeVariables t
         , Eval t v m (Base t)
         )
         => Eval t v m [] where
  eval []  = pure unit
  eval [x] = eval (project x)
  -- eval ev yield [a]    = ev pure a >>= yield
  -- eval ev yield (a:as) = do
  --   env <- askEnv :: m (Environment (LocationFor v) v)
  --   extraRoots (envRoots env (freeVariables1 as)) (ev (const (eval ev pure as)) a) >>= yield

-- Default should be to yield
-- Allow "return" to short circuit the rest of the imperative scope
