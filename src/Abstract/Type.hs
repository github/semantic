{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abstract.Type where

import Control.Effect
import Control.Monad.Effect.Internal
import Control.Monad.Fail
import Prelude hiding (fail)

type TName = Int

data Type = Int | Bool | String | NoOp | Type :-> Type | Type :* Type | TVar TName
  deriving (Eq, Ord, Show)


unify :: MonadFail m => Type -> Type -> m Type
unify Int  Int  = pure Int
unify Bool Bool = pure Bool
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify (a1 :* b1)  (a2 :* b2)  = (:*)  <$> unify a1 a2 <*> unify b1 b2
unify (TVar _) b = pure b
unify a (TVar _) = pure a
unify t1 t2 = fail ("cannot unify " ++ show t1 ++ " with " ++ show t2)


data Fresh a where
  Reset :: Int -> Fresh ()
  Fresh :: Fresh Int

class Monad m => MonadFresh m where
  fresh :: m TName
  reset :: TName -> m ()

instance (Fresh :< fs) => MonadFresh (Eff fs) where
  fresh = send Fresh
  reset = send . Reset


instance RunEffect Fresh a where
  runEffect = relayState (0 :: TName) (const pure) (\ s action k -> case action of
    Fresh -> k (succ s) s
    Reset s' -> k s' ())
