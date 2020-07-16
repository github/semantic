{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Language.Python.Failure
  ( Failure (..)
  , unimplemented
  , invariantViolated
  , eliminateFailures
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail
import Data.Coerce
import Data.Kind
import GHC.Generics (Generic1)
import Syntax.Algebra
import Syntax.Foldable
import Syntax.Functor
import Syntax.Module
import Syntax.Sum
import Syntax.Term
import Syntax.Traversable

data Failure (f :: Type -> Type) a
  = Unimplemented String
  | InvariantViolated String
    deriving Generic1

instance Show (Failure f a) where
  show (Unimplemented a)     = "unimplemented: " <> a
  show (InvariantViolated a) = "invariant violated: " <> a

deriving instance Functor (Failure f)
deriving instance Foldable (Failure f)
deriving instance Traversable (Failure f)

instance HFunctor Failure
instance HFoldable Failure
instance HTraversable Failure

instance RightModule Failure where
  a >>=* _ = coerce a


unimplemented :: (Show ast, Has Failure sig m) => ast -> m a
unimplemented = send . Unimplemented . show

invariantViolated :: Has Failure sig m => String -> m a
invariantViolated = send . InvariantViolated

eliminateFailures :: (MonadFail m, HTraversable sig, RightModule sig)
                  => Term (Failure :+: sig) a
                  -> m (Term sig a)
eliminateFailures = Syntax.Term.handle (pure . pure) (fail . show)
