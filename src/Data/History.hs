{-# LANGUAGE TypeOperators, GADTs #-}

module Data.History
  ( History (..)
  , mark
  , remark
  ) where

import Data.Location

-- | 'History' values, when attached to a given 'Term', describe the ways in
-- which that term was modified during a refactoring pass, if any.
data History
  = Refactored Location
  -- ^ A 'Refactored' node was changed by a refactor but still has
  -- (possibly-inaccurate) position information.
  | Unmodified Location
  -- ^ An 'Unmodified' node was not changed, but may have 'Refactored' children.
  deriving (Show, Eq)

-- | Convert a 'Term' annotated with a 'Range' to one annotated with a 'History'.
mark :: Functor f
  => (Location -> History)
  -> f Location
  -> f History
mark = fmap

-- | Change the 'History' annotation on a 'Term'.
remark :: Functor f
  => (Location -> History)
  -> f History
  -> f History
remark f = fmap go where
  go h = case h of
    Refactored l -> f l
    Unmodified l -> f l
