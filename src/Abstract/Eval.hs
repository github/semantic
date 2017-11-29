{-# LANGUAGE AllowAmbiguousTypes, DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Abstract.Eval
( Eval(..)
, MonadGC(..)
, MonadFail(..)
) where

import Abstract.Environment
import Abstract.FreeVariables
import Abstract.Store
import Abstract.Value
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Fail
import Data.Proxy
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import Data.Union
import Prelude hiding (fail)


-- Collecting evaluator
class Monad m => Eval v m constr where
  eval :: FreeVariables term => ((v -> m v) -> term -> m v) ->  (v -> m w) -> constr term -> m w
  default eval :: (FreeVariables term, MonadFail m) => ((v -> m v) -> term -> m v) ->  (v -> m w) -> constr term -> m w
  eval _ _ _ = fail "default eval"

instance (Monad m, Apply (Eval v m) fs) => Eval v m (Union fs) where
  eval ev yield = apply (Proxy :: Proxy (Eval v m)) (eval ev yield)

instance (Monad m, Eval v m s) => Eval v m (TermF s a) where
  eval ev yield In{..} = eval ev yield termOut


class Monad m => MonadGC l a m where
  askRoots :: m (Set.Set (Address l a))

  extraRoots :: Set.Set (Address l a) -> m b -> m b

instance (Ord l, Reader (Set.Set (Address l a)) :< fs) => MonadGC l a (Eff fs) where
  askRoots = ask :: Eff fs (Set.Set (Address l a))

  extraRoots roots' = local (<> roots')


instance ( Monad m
         , Ord (LocationFor v)
         , MonadGC (LocationFor v) v m
         , MonadEnv (LocationFor v) v m
         , AbstractValue v
         )
         => Eval v m [] where
  eval _  yield []     = yield unit
  eval ev yield [a]    = ev pure a >>= yield
  eval ev yield (a:as) = do
    env <- askEnv @(LocationFor v) @v
    extraRoots (envRoots @(LocationFor v) env (freeVariables1 as)) (ev (const (eval ev pure as)) a) >>= yield
