{-# LANGUAGE DeriveFunctor #-}
module Scope.Path
  ( Path (..)
  , pathDeclaration
  , pathDeclarationScope
  , pathPosition
  ) where

import Data.Hole
import Scope.Info
import Scope.Types

data Path scope
  = Hole
  -- | Construct a direct path to a declaration.
  | DPath Declaration Position
  -- | Construct an edge from a scope to another declaration path.
  | EPath EdgeLabel scope (Path scope)
  deriving (Eq, Functor, Ord, Show)

instance AbstractHole (Path scope) where
  hole = Hole

-- Returns the declaration of a path.
pathDeclaration :: Path scope -> Declaration
pathDeclaration (DPath d _)   = d
pathDeclaration (EPath _ _ p) = pathDeclaration p
pathDeclaration Hole          = undefined

-- TODO: Store the current scope closer _in_ the DPath?
pathDeclarationScope :: scope -> Path scope -> Maybe scope
pathDeclarationScope _ (EPath _ scope (DPath _ _)) = Just scope
pathDeclarationScope currentScope (EPath _ _ p)    = pathDeclarationScope currentScope p
pathDeclarationScope currentScope (DPath _ _)      = Just currentScope
pathDeclarationScope _ Hole                        = Nothing

-- TODO: Possibly return in Maybe since we can have Hole paths
pathPosition :: Path scope -> Position
pathPosition Hole          = Position 0
pathPosition (DPath _ p)   = p
pathPosition (EPath _ _ p) = pathPosition p
