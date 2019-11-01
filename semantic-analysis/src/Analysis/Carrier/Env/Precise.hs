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
import Control.Effect.Carrier
import Control.Effect.Fresh
import Control.Effect.Reader
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map

type Precise = Int
type Env name = Map.Map name Precise

newtype EnvC name m a = EnvC { runEnv :: m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance ( Carrier sig m
         , Member Fresh sig
         , Member (Reader (Env name)) sig
         , Ord name
         )
      => Carrier (A.Env name Precise :+: sig) (EnvC name m) where
  eff (L (A.Alloc _ k))          = fresh >>= k
  eff (L (A.Bind name addr m k)) = local (Map.insert name addr) m >>= k
  eff (L (A.Lookup name k))      = asks (Map.lookup name) >>= k
  eff (R other)                  = EnvC (eff (handleCoercible other))
