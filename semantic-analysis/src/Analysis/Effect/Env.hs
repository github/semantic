{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, LambdaCase, StandaloneDeriving #-}
module Analysis.Effect.Env
( alloc
, bind
, lookupEnv
, Env(..)
) where

import Control.Effect.Carrier

alloc :: (Carrier sig m, Member (Env name addr) sig) => name -> m addr
alloc name = send (Alloc name pure)

bind :: (Carrier sig m, Member (Env name addr) sig) => name -> addr -> m a -> m a
bind name addr m = send (Bind name addr m pure)

lookupEnv :: (Carrier sig m, Member (Env name addr) sig) => name -> m (Maybe addr)
lookupEnv name = send (Lookup name pure)


data Env name addr m k
  = Alloc name (addr -> m k)
  | forall a . Bind name addr (m a) (a -> m k)
  | Lookup name (Maybe addr -> m k)

deriving instance Functor m => Functor (Env name addr m)

instance HFunctor (Env name addr) where
  hmap f = \case
    Alloc name k -> Alloc name (f . k)
    Bind name addr m k -> Bind name addr (f m) (f . k)
    Lookup name k -> Lookup name (f . k)

instance Effect (Env name addr) where
  handle ctx hdl = \case
    Alloc name k -> Alloc name (hdl . (<$ ctx) . k)
    Bind name addr m k -> Bind name addr (hdl (m <$ ctx)) (hdl . fmap k)
    Lookup name k -> Lookup name (hdl . (<$ ctx) . k)
