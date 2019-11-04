{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Analysis.Effect.Heap
( Heap(..)
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

data Heap addr value m k
  = Deref addr (Maybe value -> m k)
  | Assign addr value (m k)
  deriving (Functor, Generic1)

instance HFunctor (Heap addr value)
instance Effect   (Heap addr value)
