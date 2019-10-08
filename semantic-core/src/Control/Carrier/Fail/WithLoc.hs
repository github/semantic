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
import Data.Loc
import Prelude hiding (fail)

runFail :: FailC m a -> m (Either (Loc, String) a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC (Loc, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig, Member (Reader Loc) sig) => MonadFail (FailC m) where
  fail s = do
    loc <- ask
    FailC (throwError (loc :: Loc, s))

instance (Carrier sig m, Effect sig, Member (Reader Loc) sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))
