{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Analysis.Effect.Domain
( -- * Domain effect
  Domain(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

data Domain term value m k
  = Abstract term (value -> m k)
  | Concretize value (term -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term value)
