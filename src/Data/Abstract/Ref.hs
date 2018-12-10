{-# LANGUAGE GADTs #-}
module Data.Abstract.Ref
  ( ValueRef (..)
  , Ref (..)
  ) where

import Data.Abstract.ScopeGraph (Slot(..))
import Data.Bifunctor
import Control.Abstract.Hole

-- | 'ValueRef' is the type subterms evaluate to and can represent either values directly ('Rval'), or references to values (lvals - such as local variables or object members)
data ValueRef address value where
  -- | A value.
  Rval       :: value -> ValueRef address value
  -- | An object member.
  LvalMember :: Slot address -> ValueRef address value

instance AbstractHole value => AbstractHole (ValueRef address value) where
  hole = Rval hole

instance Bifunctor ValueRef where
  bimap _ g (Rval v) = Rval (g v)
  bimap f _ (LvalMember slot@Slot{..}) = LvalMember (slot { frameAddress = f frameAddress })

deriving instance (Eq value, Eq address) => Eq (ValueRef address value)
deriving instance (Ord value, Ord address) => Ord (ValueRef address value)
deriving instance (Show value, Show address) => Show (ValueRef address value)

newtype Ref address value = Ref address
