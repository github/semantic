{-# LANGUAGE DeriveFunctor #-}
module Analysis.Effect.Domain
( -- * Domain effect
  Domain(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Effect.Carrier

data Domain term value m k
  = Abstract term (value -> m k)
  | Concretize value (term -> m k)
  deriving (Functor)
