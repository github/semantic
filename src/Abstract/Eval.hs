{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Eval
( Eval(..)
, MonadGC(..)
, MonadFail(..)
) where

import Abstract.Environment
import Abstract.FreeVariables
import Abstract.Monad.Env
import Abstract.Store
import Abstract.Value
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Fail
import Data.Functor.Classes
import Data.Proxy
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import Data.Union
import Prelude hiding (fail)


-- Collecting evaluator
class Monad m => Eval term v m constr where
  eval :: ((v -> m v) -> term -> m v) -> ((v -> m v) -> constr term -> m v)
  default eval :: (MonadFail m, Show1 constr) => ((v -> m v) -> term -> m v) -> ((v -> m v) -> constr term -> m v)
  eval _ _ expr = fail $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

instance (Monad m, Apply (Eval t v m) fs) => Eval t v m (Union fs) where
  eval ev yield = apply (Proxy :: Proxy (Eval t v m)) (eval ev yield)

instance (Monad m, Eval t v m s) => Eval t v m (TermF s a) where
  eval ev yield In{..} = eval ev yield termFOut


class Monad m => MonadGC a m where
  askRoots :: m (Set.Set (Address (LocationFor a) a))

  extraRoots :: Set.Set (Address (LocationFor a) a) -> m b -> m b

instance (Ord (LocationFor a), Reader (Set.Set (Address (LocationFor a) a)) :< fs) => MonadGC a (Eff fs) where
  askRoots = ask :: Eff fs (Set.Set (Address (LocationFor a) a))

  extraRoots roots' = local (<> roots')


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
