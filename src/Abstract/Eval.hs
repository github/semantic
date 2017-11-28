{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Eval where

import Abstract.FreeVariables
import Abstract.Store

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Proxy
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import Data.Union


-- Collecting evaluator
class Monad m => Eval v m constr where
  eval :: FreeVariables term => ((v -> m v) -> term -> m v) ->  (v -> m w) -> constr term -> m w
  eval = fail "default eval"

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
