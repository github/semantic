{-# LANGUAGE GADTs #-}
module Data.Abstract.Ref
  ( ValueRef (..)
  , Ref (..)
  ) where

import Data.Abstract.Name

-- | 'ValueRef' is the type subterms evaluate to and can represent either values directly ('Rval'), or references to values (lvals - such as local variables or object members)
data ValueRef address where
  -- | A value.
  Rval       :: address -> ValueRef address
  -- | A local variable. No environment is attached—it’s assumed that 'LvalLocal' will be evaluated in the same scope it was constructed in.
  LvalLocal  :: Name -> ValueRef address
  -- | An object member.
  LvalMember :: address -> Name -> ValueRef address
  deriving (Eq, Ord, Show)


newtype Ref address value = Ref address
