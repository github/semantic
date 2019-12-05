{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, ExistentialQuantification,
             FlexibleContexts, KindSignatures, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes,
             StandaloneDeriving, TypeOperators #-}

module Language.Python.Failure
  ( Failure (..)
  , unimplemented
  , invariantViolated
--  , eliminateFailures
  ) where

import Prelude hiding (fail)

import Control.Effect.Carrier
import Data.Coerce
import Data.Kind
import Syntax.Module
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

-- eliminateFailures :: forall m a sig . (MonadFail m, HFunctor sig, forall g . Functor g => Functor (sig g))
--                   => Term (Failure :+: sig) a
--                   -> m (Term sig a)
-- eliminateFailures = iter
--  _
-- eliminateFailures (Var v) = pure (Var v)
-- eliminateFailures (Alg (L _x)) = Control.Monad.Fail.fail "encountered failure"
-- eliminateFailures (Alg (R y)) = Alg <$> (htraverse eliminateFailures y)
--eliminateFailures (Alg (R other)) = Alg <$> (_hmap (eliminateFailures f) other)

-- htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g a -> f (t h a)
-- htraverse _f _x = undefined
