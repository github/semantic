{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.NonDet
( MonadNonDet(..)
, NonDetEff
) where

import Control.Applicative
import Control.Monad.Effect.Internal
import Control.Monad.Effect.NonDetEff

class (Alternative m, Monad m) => MonadNonDet m where
  collect :: Monoid b => (a -> b) -> m a -> m b

instance (NonDetEff :< fs) => MonadNonDet (Eff fs) where
  collect f = interpose (pure . f) (\ m k -> case m of
    MZero -> pure mempty
    MPlus -> mappend <$> k True <*> k False)
