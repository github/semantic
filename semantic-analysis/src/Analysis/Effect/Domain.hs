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

abstract :: (Member (Domain concrete abstract) sig, Carrier sig m) => concrete -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: (Member (Domain concrete abstract) sig, Carrier sig m) => abstract -> m concrete
concretize abstract = send (Concretize abstract pure)


data Domain concrete abstract m k
  = Abstract   concrete (abstract -> m k)
  | Concretize abstract (concrete -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain concrete abstract)
instance Effect   (Domain concrete abstract)
