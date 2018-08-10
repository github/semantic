{-# LANGUAGE TypeOperators, GADTs #-}

module Data.History
  ( History (..)
  , mark
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
  = Generated         -- ^ A 'Generated' node was created by a refactor and has no position information.
  | Refactored Range  -- ^ A 'Refactored' node was changed by a refactor but still has (possibly-inaccurate) position information.
  | Modified Range    -- ^ A 'Modified' node was not changed by a refactor, but its children may be 'Generated', 'Refactored', or 'Modified'.
  | Pristine Range    -- ^ A 'Pristine' node was not changed and has no changed (non-'Pristine') children.
  deriving (Show, Eq)

wasChanged :: History -> Bool
wasChanged (Pristine _) = False
wasChanged _ = True

-- | Convert a 'Term' annotated with a 'Range' to one annotated with a 'History'.
mark :: Functor f => (Range -> History) -> f (Record (Range ': fields)) -> f (Record (History ': fields))
mark f = fmap go where go (r :. a) = f r :. a

remark :: Functor f => (Range -> History) -> f (Record (History ': fields)) -> f (Record (History ': fields))
remark f = fmap go where
  go (r :. a) = x :. a where
    x = case r of
      Generated    -> Generated
      Refactored r -> f r
      Modified r   -> f r
      Pristine r   -> f r

-- | After a refactor has finished, it's possible that the 'History'
-- invariants may not hold. Given a current history and a list of
-- histories of the children, this provides a history result for which
-- the invariants do hold.
revise :: History -> [History] -> History
revise parent children
  | null children                   = parent
  | all (not . wasChanged) children = overwrite Pristine parent
  | any wasChanged children         = overwrite Modified parent
  | otherwise                       = parent

overwrite :: (Range -> History) -> History -> History
overwrite f (Pristine r)   = f r
overwrite f (Modified r)   = f r
overwrite f (Refactored r) = f r
overwrite _ Generated      = Generated
