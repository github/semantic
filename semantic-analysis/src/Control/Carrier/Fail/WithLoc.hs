{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Fail.WithLoc
( -- * Fail carrier
  runFail
, FailC(..)
  -- * Fail effect
, module Control.Effect.Fail
) where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.Error.Either
import           Control.Effect.Fail
import           Control.Effect.Reader
import           Prelude hiding (fail)
import           Source.Span
import qualified System.Path as Path

runFail :: FailC m a -> m (Either (Path.AbsRelFile, Span, String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (Path.AbsRelFile, Span, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m) => MonadFail (FailC m) where
  fail s = do
    path <- ask
    span <- ask
    FailC (throwError (path :: Path.AbsRelFile, span :: Span, s))

instance (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m) => Algebra (Fail :+: sig) (FailC m) where
  alg _   (L (Fail s)) _   = fail s
  alg hdl (R other)    ctx = FailC (alg (runFailC . hdl) (R other) ctx)
