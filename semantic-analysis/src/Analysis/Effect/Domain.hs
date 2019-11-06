{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Analysis.Effect.Domain
( -- * Domain effect
  abstract
, Domain(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

abstract :: (Member (Domain term value) sig, Carrier sig m) => term -> m value
abstract term = send (Abstract term pure)


data Domain term value m k
  = Abstract   term  (value -> m k)
  | Concretize value (term  -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term value)
instance Effect   (Domain term value)
