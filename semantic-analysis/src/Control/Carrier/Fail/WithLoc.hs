{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Fail.WithLoc
( -- * Fail carrier
  runFail
, FailC(..)
  -- * Reference
, Reference(..)
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

-- Fail carrier

runFail :: FailC m a -> m (Either (Reference, String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (Reference, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m) => MonadFail (FailC m) where
  fail s = do
    ref <- Reference <$> ask <*> ask
    FailC (throwError (ref, s))

instance (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m) => Algebra (Fail :+: sig) (FailC m) where
  alg _   (L (Fail s)) _   = fail s
  alg hdl (R other)    ctx = FailC (alg (runFailC . hdl) (R other) ctx)


-- Reference

data Reference = Reference
  { refPath :: Path.AbsRelFile
  , relSpan :: Span
  }
  deriving (Eq, Ord, Show)
-- FIXME: find this a better home
-- FIXME: add this to some sort of static context carried in analyses
