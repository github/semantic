{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Analysis.Carrier.Env.Monovariant
( -- * Env carrier
  EnvC(..)
  -- * Env effect
, module Analysis.Effect.Env
) where

import Analysis.Effect.Env
import Control.Effect.Carrier
import qualified Control.Monad.Fail as Fail

newtype EnvC name m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Carrier sig m
      => Carrier (Env name name :+: sig) (EnvC name m) where
  eff (L (Alloc name k))  = k name
  eff (L (Bind _ _ m k))  = m >>= k
  eff (L (Lookup name k)) = k (Just name)
  eff (R other)           = EnvC (eff (handleCoercible other))
