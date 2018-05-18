{-# LANGUAGE GADTs #-}
module Data.Abstract.Ref where

import Data.Abstract.Name

-- | 'ValueRef' is the type subterms evaluate to and can represent either values directly ('Rval'), or references to values (lvals - such as local variables or object members)
data ValueRef value where
  -- | A value.
  Rval       :: value -> ValueRef value
  -- | A local variable. No environment is attached—it’s assumed that 'LvalLocal' will be evaluated in the same scope it was constructed in.
  LvalLocal  :: Name -> ValueRef value
  -- | An object member.
  LvalMember :: value -> Name -> ValueRef value
  deriving (Eq, Ord, Show)
