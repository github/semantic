{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Scope.Info
  ( Info (..)
  ) where

import Data.Generics.Product (field)
import Data.Hole
import Data.Module
import Data.Semilattice.Lower
import GHC.Generics (Generic)
import Scope.Types
import Source.Span

data Info scopeAddress = Info
  { infoDeclaration     :: Declaration
  , infoModule          :: ModuleInfo
  , infoRelation        :: Relation
  , infoAccessControl   :: AccessControl
  , infoSpan            :: Span
  , infoKind            :: Kind
  , infoAssociatedScope :: Maybe scopeAddress
  } deriving (Eq, Show, Ord, Generic)

instance HasSpan (Info scopeAddress) where
  span_ = field @"infoSpan"
  {-# INLINE span_ #-}

instance Lower (Info scopeAddress) where
  lowerBound = Info lowerBound lowerBound lowerBound Public lowerBound lowerBound Nothing

instance AbstractHole (Info address) where
  hole = lowerBound
