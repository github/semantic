{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
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
import Analysis.Name
import Control.Effect.Carrier
import Control.Effect.Fresh
import Control.Effect.Reader
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map

type Precise = Int
type Env = Map.Map Name Precise

newtype EnvC m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance ( Carrier sig m
         , Member Fresh sig
         , Member (Reader Env) sig
         )
      => Carrier (A.Env Precise :+: sig) (EnvC m) where
  eff (L (A.Alloc _ k))          = fresh >>= k
  eff (L (A.Bind name addr m k)) = local (Map.insert name addr) m >>= k
  eff (L (A.Lookup name k))      = asks (Map.lookup name) >>= k
  eff (R other)                  = EnvC (eff (handleCoercible other))
