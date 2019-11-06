{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Analysis.Effect.Domain
( -- * Domain effect
  abstract
, concretize
, Domain(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

abstract :: (Member (Domain term value) sig, Carrier sig m) => term -> m value
abstract term = send (Abstract term pure)

concretize :: (Member (Domain term value) sig, Carrier sig m) => value -> m term
concretize value = send (Concretize value pure)


data Domain term value m k
  = Abstract   term  (value -> m k)
  | Concretize value (term  -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term value)
instance Effect   (Domain term value)
