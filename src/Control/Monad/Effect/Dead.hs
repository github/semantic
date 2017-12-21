{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Dead where

import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Semigroup
import Data.Set

-- | A set of “dead” (unreachable) terms.
newtype Dead a = Dead { unDead :: Set a }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

-- | 'Monad's offering a readable & writable set of 'Dead' terms.
class Monad m => MonadDead t m where
  killAll :: Dead t -> m ()
  revive :: Ord t => t -> m ()

instance (State (Dead t) :< fs) => MonadDead t (Eff fs) where
  killAll = put
  revive = modify . (Dead .) . (. unDead) . delete
