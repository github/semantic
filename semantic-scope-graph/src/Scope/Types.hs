{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Scope.Types
  ( Slot (..)
  , EdgeLabel (..)
  , Position (..)
  , Domain (..)
  , Kind (..)
  , AccessControl (..)
  -- * Identificatory newtypes
  , CurrentScope (..)
  ) where

import Data.Aeson (ToJSON)
import Data.Hashable
import Data.Hole
import Data.Semilattice.Lower
import GHC.Generics (Generic)

-- A slot is a location in the heap where a value is stored.
data Slot address = Slot { frameAddress :: address, position :: Position }
    deriving (Eq, Show, Ord)

instance AbstractHole address => AbstractHole (Slot address) where
  hole = Slot hole (Position 0)


-- | The type of edge from a scope to its parent scopes.
-- Either a lexical edge or an import edge in the case of non-lexical edges.
data EdgeLabel = Lexical | Import | Export | Superclass | Void
  deriving (Bounded, Enum, Eq, Ord, Show)


newtype Position = Position { unPosition :: Int }
  deriving (Eq, Show, Ord)


data Domain
  = Standard
  | Preluded
  deriving (Eq, Show, Ord)


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

-- | A newtype indicating that the wrapped datum represents a parent scope
-- in some contextual computation.
newtype CurrentScope address = CurrentScope { unCurrentScope :: address }

