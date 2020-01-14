{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, LambdaCase, StandaloneDeriving #-}
module Analysis.Effect.Env
( -- * Env effect
  alloc
, bind
, lookupEnv
, Env(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Analysis.Name
import Control.Algebra

alloc :: Has (Env addr) sig m => Name -> m addr
alloc name = send (Alloc name pure)

bind :: Has (Env addr) sig m => Name -> addr -> m a -> m a
bind name addr m = send (Bind name addr m pure)

lookupEnv :: Has (Env addr) sig m => Name -> m (Maybe addr)
lookupEnv name = send (Lookup name pure)


data Env addr m k
  = Alloc Name (addr -> m k)
  | forall a . Bind Name addr (m a) (a -> m k)
  | Lookup Name (Maybe addr -> m k)

deriving instance Functor m => Functor (Env addr m)

instance HFunctor (Env addr) where
  hmap f = \case
    Alloc name k -> Alloc name (f . k)
    Bind name addr m k -> Bind name addr (f m) (f . k)
    Lookup name k -> Lookup name (f . k)

instance Effect (Env addr) where
  thread ctx hdl = \case
    Alloc name k -> Alloc name (hdl . (<$ ctx) . k)
    Bind name addr m k -> Bind name addr (hdl (m <$ ctx)) (hdl . fmap k)
    Lookup name k -> Lookup name (hdl . (<$ ctx) . k)
