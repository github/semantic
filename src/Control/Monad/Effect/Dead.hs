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
  -- | Update the current 'Dead' set.
  killAll :: Dead t -> m ()

  -- | Revive a single term, removing it from the current 'Dead' set.
  revive :: Ord t => t -> m ()

instance (State (Dead t) :< fs) => MonadDead t (Eff fs) where
  killAll = put
  revive t = modify (Dead . delete t . unDead)
