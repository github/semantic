{-# LANGUAGE RankNTypes #-}
module Analysis.Analysis
( Analysis(..)
) where

import Core.Name
import Data.Text (Text)

-- | A record of functions necessary to perform analysis.
--
-- This is intended to be replaced with a selection of algebraic effects providing these interfaces and carriers providing reusable implementations.
data Analysis term address value m = Analysis
  { alloc     :: Name -> m address
  , bind      :: forall a . Name -> address -> m a -> m a
  , lookupEnv :: Name -> m (Maybe address)
  , deref     :: address -> m (Maybe value)
  , assign    :: address -> value -> m ()
  , abstract  :: (term -> m value) -> Name -> term -> m value
  , apply     :: (term -> m value) -> value -> value -> m value
  , unit      :: m value
  , bool      :: Bool -> m value
  , asBool    :: value -> m Bool
  , string    :: Text -> m value
  , asString  :: value -> m Text
  , record    :: [(Name, value)] -> m value
  , (...)     :: address -> Name -> m (Maybe address)
  }
