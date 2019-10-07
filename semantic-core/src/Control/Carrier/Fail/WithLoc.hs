{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Fail.WithLoc
( -- * Fail effect
  module Control.Effect.Fail
  -- * Fail carrier
, runFailWithLoc
, FailWithLocC(..)
) where

import Control.Applicative
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Fail
import Control.Effect.Reader
import Data.Loc
import Prelude hiding (fail)

runFailWithLoc :: FailWithLocC m a -> m (Either (Loc, String) a)
runFailWithLoc = runError . runFailWithLocC

newtype FailWithLocC m a = FailWithLocC { runFailWithLocC :: ErrorC (Loc, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig, Member (Reader Loc) sig) => MonadFail (FailWithLocC m) where
  fail s = do
    loc <- ask
    FailWithLocC (throwError (loc :: Loc, s))

instance (Carrier sig m, Effect sig, Member (Reader Loc) sig) => Carrier (Fail :+: sig) (FailWithLocC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailWithLocC (eff (R (handleCoercible other)))
