{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Carrier.Env.Monovariant
( -- * Env carrier
  EnvC(..)
  -- * Env effect
, module Analysis.Effect.Env
) where

import           Analysis.Effect.Env
import           Analysis.Name
import           Control.Algebra
import qualified Control.Monad.Fail as Fail

newtype EnvC m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Algebra sig m
      => Algebra (Env Name :+: sig) (EnvC m) where
  alg hdl sig ctx = case sig of
    L (Bind _ _ m)  -> hdl (m <$ ctx)
    L (Lookup name) -> pure (Just name <$ ctx)
    R other         -> EnvC (alg (runEnv . hdl) other ctx)
