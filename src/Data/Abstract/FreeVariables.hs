{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}
module Data.Abstract.FreeVariables where

import Data.ByteString (ByteString)
import Data.Functor.Foldable
import Data.Proxy
import Data.Set
import Data.Term
import Data.Union

-- | The type of variable names.
type Name = ByteString


-- | Types which can contain unbound variables.
class FreeVariables term where
  -- | The set of free variables in the given value.
  freeVariables :: term -> Set Name


-- | A lifting of 'FreeVariables' to type constructors of kind @* -> *@.
class FreeVariables1 syntax where
  -- | Lift a function mapping each element to its set of free variables through a containing structure, collecting the results into a single set.
  liftFreeVariables :: (a -> Set Name) -> syntax a -> Set Name
  default liftFreeVariables :: (Foldable syntax) => (a -> Set Name) -> syntax a -> Set Name
  liftFreeVariables = foldMap

freeVariables1 :: (FreeVariables1 t, FreeVariables a) => t a -> Set Name
freeVariables1 = liftFreeVariables freeVariables

instance (FreeVariables1 syntax, Functor syntax) => FreeVariables (Term syntax ann) where
  freeVariables = cata (liftFreeVariables id)

instance (FreeVariables1 syntax) => FreeVariables1 (TermF syntax ann) where
  liftFreeVariables f (In _ s) = liftFreeVariables f s

instance (Apply FreeVariables1 fs) => FreeVariables1 (Union fs) where
  liftFreeVariables f = apply (Proxy :: Proxy FreeVariables1) (liftFreeVariables f)

instance FreeVariables1 []
