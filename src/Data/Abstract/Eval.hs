{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Eval
( Eval(..)
, MonadGC(..)
, MonadFail(..)
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
import Prelude hiding (fail)


-- | The 'Eval' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Monad m => Eval term v m constr where
  -- | Evaluate a term using an open-recursive evaluator for any child terms.
  eval :: ((v -> m v) -> term -> m v)
       -> (v -> m v)
       -> constr term
       -> m v

  default eval :: (MonadFail m, Show1 constr) => ((v -> m v) -> term -> m v) -> ((v -> m v) -> constr term -> m v)
  eval _ _ expr = fail $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

instance (Monad m, Apply (Eval t v m) fs) => Eval t v m (Union fs) where
  eval ev yield = apply (Proxy :: Proxy (Eval t v m)) (eval ev yield)

instance (Monad m, Eval t v m s) => Eval t v m (TermF s a) where
  eval ev yield In{..} = eval ev yield termFOut


instance ( Monad m
         , Ord (LocationFor v)
         , MonadGC v m
         , MonadEnv v m
         , AbstractValue v
         , FreeVariables t
         )
         => Eval t v m [] where
  eval _  yield []     = yield unit
  eval ev yield [a]    = ev pure a >>= yield
  eval ev yield (a:as) = do
    env <- askEnv :: m (Environment (LocationFor v) v)
    extraRoots (envRoots env (freeVariables1 as)) (ev (const (eval ev pure as)) a) >>= yield
