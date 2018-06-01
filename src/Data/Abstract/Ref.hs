{-# LANGUAGE GADTs #-}
module Data.Abstract.Ref where

import Data.Abstract.Name

-- | 'ValueRef' is the type subterms evaluate to and can represent either values directly ('Rval'), or references to values (lvals - such as local variables or object members)
data ValueRef address value where
  -- | A value.
  Rval       :: address -> ValueRef address value
  -- | A local variable. No environment is attached—it’s assumed that 'LvalLocal' will be evaluated in the same scope it was constructed in.
  LvalLocal  :: Name -> ValueRef address value
  -- | An object member.
  LvalMember :: address -> Name -> ValueRef address value
  deriving (Eq, Ord, Show)


newtype Ref address value = Ref address
