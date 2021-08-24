{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Carrier.Env.Precise
( -- * Env carrier
  EnvC(..)
  -- * Env effect
, module Analysis.Effect.Env
) where

import           Analysis.Effect.Env
import           Analysis.Name
import           Control.Algebra
import           Control.Effect.Reader
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map

type Precise = Int
type PreciseEnv = Map.Map Name Precise

newtype EnvC m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Has (Reader PreciseEnv) sig m
      => Algebra (Env Precise :+: sig) (EnvC m) where
  alg hdl sig ctx = case sig of
    L (Bind name addr m) -> local (Map.insert name addr) (hdl (m <$ ctx))
    L (Lookup name)      -> (<$ ctx) <$> asks (Map.lookup name)
    R other              -> EnvC (alg (runEnv . hdl) other ctx)
