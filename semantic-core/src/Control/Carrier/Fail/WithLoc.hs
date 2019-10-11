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
import Core.Loc
import Prelude hiding (fail)
import Source.Span

runFail :: FailC m a -> m (Either (Path, Span, String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (Path, Span, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig, Member (Reader Path) sig, Member (Reader Span) sig) => MonadFail (FailC m) where
  fail s = do
    path <- ask
    span <- ask
    FailC (throwError (path :: Path, span :: Span, s))

instance (Carrier sig m, Effect sig, Member (Reader Path) sig, Member (Reader Span) sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))
