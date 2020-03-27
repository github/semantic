{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Scope.Info
  ( Info (..)
  , Declaration (..)
  , formatDeclaration
  , Relation (..)
  , Kind (..)
  , AccessControl (..)
  ) where

import Analysis.Name
import Data.Generics.Product (field)
import Data.Hole
import Data.Module
import Data.Semilattice.Lower
import Data.Text (Text)
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
  lowerBound = Info lowerBound lowerBound lowerBound Public (point (Pos 0 0)) lowerBound Nothing

instance AbstractHole (Info address) where
  hole = lowerBound

newtype Declaration = Declaration { unDeclaration :: Name }
  deriving (Eq, Ord, Show)

instance Lower Declaration where
  lowerBound = Declaration $ name ""

formatDeclaration :: Declaration -> Text
formatDeclaration = formatName . unDeclaration


data Relation = Default | Instance | Prelude | Gensym
  deriving (Bounded, Enum, Eq, Show, Ord)

instance Lower Relation where
  lowerBound = Default


