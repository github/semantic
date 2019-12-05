{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, ExistentialQuantification,
             FlexibleContexts, KindSignatures, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes,
             StandaloneDeriving, TypeOperators #-}

module Language.Python.Failure
  ( Failure (..)
  , unimplemented
  , invariantViolated
  , eliminateFailures
  ) where

import Prelude hiding (fail)

import Control.Effect.Carrier
import Control.Monad.Fail
import Data.Coerce
import Data.Kind
import Syntax.Module
import Syntax.Term
import Syntax.Traversable

data Failure (f :: Type -> Type) a
  = forall a . Show a => Unimplemented a
  | InvariantViolated String

instance Show (Failure f a) where
  show (Unimplemented a)     = "unimplemented: " <> show a
  show (InvariantViolated a) = "invariant violated: " <> a

deriving instance Functor (Failure f)
deriving instance Foldable (Failure f)
deriving instance Traversable (Failure f)

instance HFunctor Failure where hmap _ = coerce

instance HTraversable Failure where
  htraverse _ = \case
    Unimplemented x -> pure (Unimplemented x)
    InvariantViolated y -> pure (InvariantViolated y)

instance RightModule Failure where
  a >>=* _ = coerce a

unimplemented :: (Show ast, Member Failure sig, Carrier sig m) => ast -> m a
unimplemented x = send . Unimplemented $ x

invariantViolated :: (Member Failure sig, Carrier sig m) => String -> m a
invariantViolated = send . InvariantViolated

eliminateFailures :: (MonadFail m, HTraversable sig, RightModule sig)
                  => Term (Failure :+: sig) a
                  -> Either String (Term sig a)
eliminateFailures = Syntax.Term.handle (pure . pure) (fail . show)
