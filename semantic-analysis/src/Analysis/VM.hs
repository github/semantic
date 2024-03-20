{-# LANGUAGE FlexibleContexts #-}
module Analysis.VM
( -- * Macro-expressible syntax
  let'
, letrec
  -- * Re-exports
, module Analysis.Effect.Env
, module Analysis.Effect.Store
) where

import Analysis.Effect.Env
import Analysis.Effect.Store
import Analysis.Name
import Control.Effect.Labelled

-- Macro-expressible syntax

let' :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m) => Name -> val -> m a -> m a
let' n v m = do
  addr <- alloc n
  addr .= v
  bind n addr m

letrec :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m) => Name -> m val -> m val
letrec n m = do
  addr <- alloc n
  v <- bind n addr m
  addr .= v
  pure v
