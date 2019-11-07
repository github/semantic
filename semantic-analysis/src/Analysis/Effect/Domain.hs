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

import Analysis.Intro
import Control.Effect.Carrier
import GHC.Generics (Generic1)

abstract :: (Member (Domain abstract) sig, Carrier sig m) => Intro -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: (Member (Domain abstract) sig, Carrier sig m) => abstract -> m Intro
concretize abstract = send (Concretize abstract pure)


data Domain abstract m k
  = Abstract   Intro    (abstract -> m k)
  | Concretize abstract (Intro    -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain abstract)
instance Effect   (Domain abstract)
