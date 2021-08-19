{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Core.Eval
( eval0
, eval
) where

import Analysis.Effect.Domain
import Analysis.Effect.Env as Env
import Analysis.Effect.Store
import Control.Effect.Labelled
import Core.Expr
import Data.Function (fix)

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m) => Expr -> m val
eval0 = fix eval

eval :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m) => (Expr -> m val) -> (Expr -> m val)
eval eval = \case
  Var n     -> do
    a <- lookupEnv n
    maybe (dvar n) fetch a
  Abs n b   -> dabs n (\ v -> do
    a <- alloc n
    a .= v
    Env.bind n a (eval b))
  App f a   -> do
    f' <- eval f
    a' <- eval a
    dapp f' a'
  Let n v b -> do
    a <- alloc n
    v' <- eval v
    a .= v'
    Env.bind n a (eval b)
  Lit i -> dint i
  If c t e  -> do
    c' <- eval c
    dif c' (eval t) (eval e)
  Die s -> ddie s
