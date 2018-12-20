module Data.Abstract.Visibilities.Visibilities
  ( Visibilities (..)
  , Visibilities1 (..)
  ) where

import Data.Abstract.ScopeGraph (Visibility(..))

class Visibilities syntax where
  termToVisibility :: syntax -> Maybe Visibility
  termToVisibility = const Nothing

class Visibilities1 syntax where
  -- | Lift a function mapping a Visibility syntax to its related Relation.
  -- This can be used to find the Relation of a component syntax whose Visibility may be contained within a subterm.
  --
  -- Note that not all syntax will have contain Visibility syntax; in general it’s reserved for syntax where visibility is necessary for correctly resolving lookups of declarations (e.g. Method, PublicField, and other members whose containing syntax (i.e. Class, Module, etc.) can restrict visibility to external members).
  liftTermToVisibility :: (a -> Maybe Visibility) -> syntax a -> Maybe Visibility
  liftTermToVisibility _ _ = Nothing
