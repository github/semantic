{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Declarations  where

import Data.Abstract.FreeVariables
import Data.Term
import Prologue

class Declarations syntax where
  declaredName :: syntax -> Maybe Name
  declaredName = const Nothing

class Declarations1 syntax where
  -- | Lift a function mapping each element to its set of free variables through a containing structure, collecting the results into a single set.
  liftDeclaredName :: (a -> [Name]) -> syntax a -> Maybe Name
  liftDeclaredName _ _ = Nothing

instance Declarations t => Declarations (Subterm t a) where
  declaredName = declaredName . subterm

instance (FreeVariables1 syntax, Declarations1 syntax, Functor syntax) => Declarations (Term syntax ann) where
  declaredName = liftDeclaredName freeVariables . termOut

instance (Apply Declarations1 fs) => Declarations1 (Union fs) where
  liftDeclaredName f = apply (Proxy :: Proxy Declarations1) (liftDeclaredName f)

instance Declarations1 []
