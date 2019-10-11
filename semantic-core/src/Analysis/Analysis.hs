{-# LANGUAGE RankNTypes #-}
module Analysis.Analysis
( Analysis(..)
) where

import Data.Text (Text)

-- | A record of functions necessary to perform analysis.
--
-- This is intended to be replaced with a selection of algebraic effects providing these interfaces and carriers providing reusable implementations.
data Analysis term name address value m = Analysis
  { alloc     :: name -> m address
  , bind      :: forall a . name -> address -> m a -> m a
  , lookupEnv :: name -> m (Maybe address)
  , deref     :: address -> m (Maybe value)
  , assign    :: address -> value -> m ()
  , abstract  :: (term name -> m value) -> name -> term name -> m value
  , apply     :: (term name -> m value) -> value -> value -> m value
  , unit      :: m value
  , bool      :: Bool -> m value
  , asBool    :: value -> m Bool
  , string    :: Text -> m value
  , asString  :: value -> m Text
  , record    :: [(name, value)] -> m value
  , (...)     :: address -> name -> m (Maybe address)
  }
