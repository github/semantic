{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Analysis.Effect.Heap
( deref
, Heap(..)
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

deref :: (Member (Heap addr value) sig, Carrier sig m) => addr -> m (Maybe value)
deref addr = send (Deref addr pure)


data Heap addr value m k
  = Deref addr (Maybe value -> m k)
  | Assign addr value (m k)
  deriving (Functor, Generic1)

instance HFunctor (Heap addr value)
instance Effect   (Heap addr value)
