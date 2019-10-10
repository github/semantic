{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Fail.WithFile
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
import Data.Loc
import Data.File
import Prelude hiding (fail)
import Source.Span

runFail :: FailC m a -> m (Either (File String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (File String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig, Member (Reader Path) sig, Member (Reader Span) sig) => MonadFail (FailC m) where
  fail s = File <$> ask <*> ask <*> pure s >>= FailC . throwError

instance (Carrier sig m, Effect sig, Member (Reader Path) sig, Member (Reader Span) sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))
