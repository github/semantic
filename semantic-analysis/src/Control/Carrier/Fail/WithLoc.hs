{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Fail.WithLoc
( -- * Fail effect
  module Control.Effect.Fail
  -- * Fail carrier
, runFail
, FailC(..)
) where

import Control.Applicative
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Fail (Fail(..), MonadFail(..))
import Control.Effect.Reader
import Prelude hiding (fail)
import Source.Span
import qualified System.Path as Path

runFail :: FailC m a -> m (Either (Path.AbsRelFile, Span, String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (Path.AbsRelFile, Span, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig) => MonadFail (FailC m) where
  fail s = do
    path <- ask
    span <- ask
    FailC (throwError (path :: Path.AbsRelFile, span :: Span, s))

instance (Carrier sig m, Effect sig, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))
