{-# LANGUAGE TypeOperators, GADTs #-}

module Data.History
  ( History (..)
  , mark
  , remark
  ) where

import Source.Loc

-- | 'History' values, when attached to a given 'Term', describe the ways in
-- which that term was modified during a refactoring pass, if any.
data History
  = Refactored Range
  -- ^ A 'Refactored' node was changed by a refactor but still has
  -- (possibly-inaccurate) position information.
  | Unmodified Range
  -- ^ An 'Unmodified' node was not changed, but may have 'Refactored' children.
  deriving (Show, Eq)

-- | Convert a 'Term' annotated with a 'Range' to one annotated with a 'History'.
mark :: Functor f
  => (Range -> History)
  -> f Loc
  -> f History
mark f = fmap (f . byteRange)

-- | Change the 'History' annotation on a 'Term'.
remark :: Functor f
  => (Range -> History)
  -> f History
  -> f History
remark f = fmap go where
  go h = case h of
    Refactored l -> f l
    Unmodified l -> f l
