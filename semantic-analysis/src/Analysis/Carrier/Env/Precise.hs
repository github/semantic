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
, A.alloc
, A.bind
, A.lookupEnv
, A.Env(..)
) where

import qualified Analysis.Effect.Env as A
import           Analysis.Name
import           Control.Algebra
import           Control.Effect.Fresh
import           Control.Effect.Reader
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map

type Precise = Int
type Env = Map.Map Name Precise

newtype EnvC m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance ( Has Fresh sig m
         , Has (Reader Env) sig m
         )
      => Algebra (A.Env Precise :+: sig) (EnvC m) where
  alg hdl sig ctx = case sig of
    L (A.Alloc _)          -> (<$ ctx) <$> fresh
    L (A.Bind name addr m) -> local (Map.insert name addr) (hdl (m <$ ctx))
    L (A.Lookup name)      -> (<$ ctx) <$> asks (Map.lookup name)
    R other                -> EnvC (alg (runEnv . hdl) other ctx)
