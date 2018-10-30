{-# LANGUAGE GADTs #-}
module Data.Abstract.Ref
  ( ValueRef (..)
  , Ref (..)
  ) where

import Data.Abstract.Name
import Data.Abstract.ScopeGraph (Address)

-- | 'ValueRef' is the type subterms evaluate to and can represent either values directly ('Rval'), or references to values (lvals - such as local variables or object members)
data ValueRef address value where
  -- | A value.
  Rval       :: value -> ValueRef address value
  -- | A local variable. No environment is attached—it’s assumed that 'LvalLocal' will be evaluated in the same scope it was constructed in.
  LvalLocal  :: Name -> ValueRef address value
  -- | An object member.
  LvalMember :: Address address -> ValueRef address value


deriving instance (Eq value, Eq address) => Eq (ValueRef address value)
deriving instance (Ord value, Ord address) => Ord (ValueRef address value)
deriving instance (Show value, Show address) => Show (ValueRef address value)

newtype Ref address value = Ref address
