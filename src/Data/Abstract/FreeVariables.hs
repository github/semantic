{-# LANGUAGE DefaultSignatures, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeApplications, UndecidableInstances #-}
module Data.Abstract.FreeVariables
  ( FreeVariables (..)
  , FreeVariables1 (..)
  ) where

import Data.Abstract.Name
import           Data.Sum
import           Data.Term
import           Prologue

-- | Types which can contain unbound variables.
class FreeVariables term where
  -- | The set of free variables in the given value.
  freeVariables :: term -> Set Name


-- | A lifting of 'FreeVariables' to type constructors of kind @* -> *@.
--
--   'Foldable' types requiring no additional semantics to the set of free variables (e.g. types which do not bind any variables) can use (and even derive, with @-XDeriveAnyClass@) the default implementation.
class FreeVariables1 syntax where
  -- | Lift a function mapping each element to its set of free variables through a containing structure, collecting the results into a single set.
  liftFreeVariables :: (a -> Set Name) -> syntax a -> Set Name
  default liftFreeVariables :: (Foldable syntax) => (a -> Set Name) -> syntax a -> Set Name
  liftFreeVariables = foldMap

deriving instance FreeVariables1 syntax => FreeVariables (Term syntax ann)

instance (FreeVariables recur, FreeVariables1 syntax) => FreeVariables (TermF syntax ann recur) where
  freeVariables = liftFreeVariables freeVariables

instance FreeVariables1 syntax => FreeVariables1 (TermF syntax ann) where
  liftFreeVariables f (In _ s) = liftFreeVariables f s

instance Apply FreeVariables1 fs => FreeVariables1 (Sum fs) where
  liftFreeVariables f = apply @FreeVariables1 (liftFreeVariables f)

instance FreeVariables1 []
