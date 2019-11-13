{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable, ExistentialQuantification, FlexibleContexts, KindSignatures, RankNTypes, StandaloneDeriving #-}

module Language.Python.Failure
  ( Failure (..)
  , unimplemented
  , invariantViolated
  ) where

import Control.Effect.Carrier
import Data.Coerce
import Data.Kind

data Failure (f :: Type -> Type) a
  = forall a . Show a => Unimplemented a
  | InvariantViolated String

deriving instance Functor (Failure f)

instance HFunctor Failure where hmap _ = coerce

unimplemented :: (Show ast, Member Failure sig, Carrier sig m) => ast -> m a
unimplemented x = send . Unimplemented $ x

invariantViolated :: (Member Failure sig, Carrier sig m) => String -> m a
invariantViolated = send . InvariantViolated
