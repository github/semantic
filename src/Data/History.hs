{-# LANGUAGE TypeOperators, GADTs #-}

module Data.History
  ( History (..)
  , mark
  , markUnmodified
  , remark
  , revise
  , overwrite
  ) where

import Data.Record
import Data.Range

-- | 'History' values, when attached to a given 'Term', describe the
-- ways in which that term was modified during a refactoring pass, if
-- any.
--
-- TODO: investigate how this is congruent with our diffing
-- strategies.
data History
  = Refactored Range  -- ^ A 'Refactored' node was changed by a refactor but still has (possibly-inaccurate) position information.
  | Unmodified Range    -- ^ A 'Unmodified' node was not changed, but maybe have 'Refactored' children.
  deriving (Show, Eq)

-- | Convert a 'Term' annotated with a 'Range' to one annotated with a 'History'.
mark :: Functor f => (Range -> History) -> f (Record (Range ': fields)) -> f (Record (History ': fields))
mark f = fmap go where go (r :. a) = f r :. a

-- | Covert a 'Term' annotated with a 'Range' to one annotated with 'Unmodified' 'History'.
markUnmodified :: Functor f => f (Record (Range ': fields)) -> f (Record (History ': fields))
markUnmodified = mark Unmodified

remark :: Functor f => (Range -> History) -> f (Record (History ': fields)) -> f (Record (History ': fields))
remark f = fmap go where
  go (r :. a) = x :. a where
    x = case r of
      Refactored r -> f r
      Unmodified r -> f r


-- TODO: This can all go away now:

-- | After a refactor has finished, it's possible that the 'History'
-- invariants may not hold. Given a current history and a list of
-- histories of the children, this provides a history result for which
-- the invariants do hold.
revise :: History -> [History] -> History
revise parent children
  | null children                   = parent
  | all (not . wasChanged) children = overwrite Unmodified parent
  | any wasChanged children         = overwrite Refactored parent
  | otherwise                       = parent

overwrite :: (Range -> History) -> History -> History
overwrite f (Unmodified r)   = f r
overwrite f (Refactored r) = f r

wasChanged :: History -> Bool
wasChanged (Unmodified _) = False
wasChanged _ = True
