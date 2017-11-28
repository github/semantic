{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Abstract.Eval where

import Abstract.Store
import Data.Proxy
import Data.Term
import Data.Union

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semigroup


-- Collecting evaluator
class Monad m => Eval l v m term constr where
  eval :: ((v -> m v) -> term -> m v) ->  (v -> m w) -> constr term -> m w
  eval = fail "default eval"

instance (Monad m, Apply (Eval l v m t) fs) => Eval l v m t (Union fs) where
  eval ev yield = apply (Proxy :: Proxy (Eval l v m t)) (eval @l ev yield)

instance (Monad m, Eval l v m t s) => Eval l v m t (TermF s a) where
  eval ev yield In{..} = eval @l ev yield termOut


class Monad m => MonadGC l a m where
  askRoots :: m (Set (Address l a))

  extraRoots :: Set (Address l a) -> m b -> m b

instance (Ord l, Reader (Set (Address l a)) :< fs) => MonadGC l a (Eff fs) where
  askRoots = ask :: Eff fs (Set (Address l a))

  extraRoots roots' = local (<> roots')
