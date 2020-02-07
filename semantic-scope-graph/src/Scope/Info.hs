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
import Data.Aeson (ToJSON)
import Data.Generics.Product (field)
import Data.Hashable (Hashable)
import Data.Hole
import Data.Module
import Data.Semilattice.Lower
import Data.Text (Text)
import GHC.Generics (Generic)
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


data Kind = AbstractClass
          | Assignment
          | Call
          | Class
          | DefaultExport
          | Function
          | Identifier
          | Let
          | MemberAccess
          | Method
          | Module
          | New
          | Parameter
          | PublicField
          | QualifiedAliasedImport
          | QualifiedExport
          | QualifiedImport
          | RequiredParameter
          | This
          | TypeAlias
          | TypeIdentifier
          | Unknown
          | UnqualifiedImport
          | VariableDeclaration
  deriving (Bounded, Enum, Eq, Show, Ord)

instance Lower Kind where
  lowerBound = Unknown


data AccessControl = Public
                   | Protected
                   | Private
                   deriving (Bounded, Enum, Eq, Generic, Hashable, ToJSON, Show)

-- | The Ord AccessControl instance represents an order specification of AccessControls.
-- AccessControls that are less than or equal to another AccessControl implies access.
-- It is helpful to consider `Public <= Private` as saying "Can a Public syntax term access a Private syntax term?"
-- In this way, Public AccessControl is the top of the order specification, and Private AccessControl is the bottom.
instance Ord AccessControl where
  -- | Private AccessControl represents the least overlap or accessibility with other AccessControls.
  -- When asking if the AccessControl "on the left" is less than the AccessControl "on the right", Private AccessControl on the left always implies access to the thing on the right.
  (<=) Private _           = True
  (<=) _       Private     = False

  -- | Protected AccessControl is in between Private and Public in the order specification.
  -- Protected AccessControl "on the left" has access to Protected or Public AccessControls "on the right".
  (<=) Protected Public    = True
  (<=) Protected Protected = True

  -- | Public AccessControl "on the left" has access only to Public AccessControl "on the right".
  (<=) Public Public       = True
  (<=) Public _            = False

