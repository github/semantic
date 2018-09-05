{-# LANGUAGE TypeOperators, GADTs #-}

module Data.History
  ( History (..)
  , mark
  , remark
  ) where

import Data.Record
import Data.Range

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
  -> f (Record (Range ': fields))
  -> f (Record (History ': fields))
mark f = fmap go where go (r :. a) = f r :. a

-- | Change the 'History' annotation on a 'Term'.
remark :: Functor f
  => (Range -> History)
  -> f (Record (History ': fields))
  -> f (Record (History ': fields))
remark f = fmap go where
  go (r :. a) = x :. a where
    x = case r of
      Refactored r -> f r
      Unmodified r -> f r
