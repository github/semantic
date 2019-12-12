{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Analysis.Effect.Domain
( -- * Domain effect
  abstract
, concretize
, Domain(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Analysis.Intro
import Control.Algebra
import GHC.Generics (Generic1)

abstract :: Has (Domain abstract) sig m => Intro -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: Has (Domain abstract) sig m => abstract -> m Intro
concretize abstract = send (Concretize abstract pure)


data Domain abstract m k
  = Abstract   Intro    (abstract -> m k)
  | Concretize abstract (Intro    -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain abstract)
instance Effect   (Domain abstract)
