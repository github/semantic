{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}
module Abstract.FreeVariables where

import Abstract.Set
import Data.Term
import Data.Proxy
import Data.Union
import Data.Functor.Foldable

type Name = String

class FreeVariables1 syntax where
  liftFreeVariables :: (a -> Set Name) -> syntax a -> Set Name
  default liftFreeVariables :: (Foldable syntax) => (a -> Set Name) -> syntax a -> Set Name
  liftFreeVariables = foldMap

class FreeVariables term where
  freeVariables :: term -> Set Name

instance (FreeVariables1 (TermF syntax ann), Functor syntax) => FreeVariables (Term syntax ann) where
  freeVariables = cata (liftFreeVariables id)

instance (Apply FreeVariables1 fs) => FreeVariables1 (Union fs) where
  liftFreeVariables f = apply (Proxy :: Proxy FreeVariables1) (liftFreeVariables f)
