{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable, ExistentialQuantification, FlexibleContexts, KindSignatures, RankNTypes, StandaloneDeriving, TypeOperators, QuantifiedConstraints #-}

module Language.Python.Failure
  ( Failure (..)
  , unimplemented
  , invariantViolated
  , eliminateFailures
  ) where

import Prelude hiding (fail)

import Control.Effect.Carrier
import Data.Coerce
import Data.Kind
import Syntax.Term

data Failure (f :: Type -> Type) a
  = forall a . Show a => Unimplemented a
  | InvariantViolated String

instance Show (Failure f a) where
  show (Unimplemented a) = "unimplemented: " <> show a
  show (InvariantViolated a) = "invariant violated: " <> a

deriving instance Functor (Failure f)

instance HFunctor Failure where hmap _ = coerce

unimplemented :: (Show ast, Member Failure sig, Carrier sig m) => ast -> m a
unimplemented x = send . Unimplemented $ x

invariantViolated :: (Member Failure sig, Carrier sig m) => String -> m a
invariantViolated = send . InvariantViolated

eliminateFailures :: forall a sig . (HFunctor sig, forall g . Functor g => Functor (sig g))
                  => (forall x y . Failure x y -> Term sig y)
                  -> Term (Failure :+: sig) a
                  -> Term sig a
eliminateFailures _ (Var v) = Var v
eliminateFailures f (Alg (L x)) = f x
eliminateFailures f (Alg (R other)) = Alg (hmap (eliminateFailures f) other)

