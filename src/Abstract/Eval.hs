{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, DefaultSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
module Abstract.Eval where

import Abstract.Store
import Data.Term
import Data.Proxy
import Data.Union


-- Standard evaluator/interpreter
class Monad m => Eval v m syntax ann constr where
  evaluate :: (Term syntax ann -> m v) -> constr (Term syntax ann) -> m v


instance ( Monad m
         , Apply (Eval v m s a) fs
         )
         => Eval v m s a (Union fs) where
  evaluate ev = apply (Proxy :: Proxy (Eval v m s a)) (evaluate ev)

instance (Monad m, Eval v m s a s) => Eval v m s a (TermF s a) where
  evaluate ev In{..} = evaluate ev termOut

-- Collecting evaluator
class Monad m => EvalCollect l v m syntax ann constr where
  evalCollect :: (Term syntax ann -> m v)
              -> constr (Term syntax ann)
              -> m v
  default evalCollect :: (Eval v m syntax ann constr) => (Term syntax ann -> m v)
                      -> constr (Term syntax ann)
                      -> m v
  evalCollect = evaluate

instance ( Monad m
         , Apply (EvalCollect l v m s a) fs
         )
         => EvalCollect l v m s a (Union fs) where
  evalCollect ev = apply (Proxy :: Proxy (EvalCollect l v m s a)) (evalCollect @l ev)


class Monad m => MonadGC l a m where
  askRoots :: m (Set (Address l a))

  extraRoots :: Set (Address l a) -> m b -> m b
