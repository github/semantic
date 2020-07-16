{-# LANGUAGE GADTs #-}
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
alloc name = send (Alloc name)

bind :: Has (Env addr) sig m => Name -> addr -> m a -> m a
bind name addr m = send (Bind name addr m)

lookupEnv :: Has (Env addr) sig m => Name -> m (Maybe addr)
lookupEnv name = send (Lookup name)


data Env addr m k where
  Alloc  :: Name ->                Env addr m addr
  Bind   :: Name -> addr -> m a -> Env addr m a
  Lookup :: Name ->                Env addr m (Maybe addr)
