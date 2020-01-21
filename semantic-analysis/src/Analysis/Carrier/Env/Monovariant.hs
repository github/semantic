{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Analysis.Carrier.Env.Monovariant
( -- * Env carrier
  EnvC(..)
  -- * Env effect
, module Analysis.Effect.Env
) where

import Analysis.Effect.Env
import Analysis.Name
import Control.Algebra
import qualified Control.Monad.Fail as Fail

newtype EnvC m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Algebra sig m
      => Algebra (Env Name :+: sig) (EnvC m) where
  alg (L (Alloc name k))  = k name
  alg (L (Bind _ _ m k))  = m >>= k
  alg (L (Lookup name k)) = k (Just name)
  alg (R other)           = EnvC (alg (handleCoercible other))
