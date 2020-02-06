{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Fail.WithLoc
( -- * Fail effect
  module Control.Effect.Fail
  -- * Fail carrier
, runFail
, FailC(..)
) where

import Control.Applicative
import Control.Algebra
import Control.Carrier.Error.Either
import Control.Effect.Fail
import Control.Effect.Reader
import Prelude hiding (fail)
import Source.Span
import qualified System.Path as Path

runFail :: FailC m a -> m (Either (Path.AbsRelFile, Span, String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (Path.AbsRelFile, Span, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Effect sig, Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m) => MonadFail (FailC m) where
  fail s = do
    path <- ask
    span <- ask
    FailC (throwError (path :: Path.AbsRelFile, span :: Span, s))

instance (Effect sig, Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m) => Algebra (Fail :+: sig) (FailC m) where
  alg (L (Fail s)) = fail s
  alg (R other)    = FailC (alg (R (handleCoercible other)))
