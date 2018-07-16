{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Data.Abstract.Declarations  where

import Data.Abstract.Name
import Data.Sum
import Data.Term
import Prologue

class Declarations syntax where
  declaredName :: syntax -> Maybe Name
  declaredName = const Nothing

class Declarations1 syntax where
  -- | Lift a function mapping each element to its declared name (if any) through a containing structure. This can be used to define the declared name for a composite piece of syntax in terms of the declared name of one of its components.
  liftDeclaredName :: (a -> Maybe Name) -> syntax a -> Maybe Name
  liftDeclaredName _ _ = Nothing

instance Declarations t => Declarations (Subterm t a) where
  declaredName = declaredName . subterm

deriving instance Declarations1 syntax => Declarations (Term syntax ann)

instance (Declarations recur, Declarations1 syntax) => Declarations (TermF syntax ann recur) where
  declaredName = liftDeclaredName declaredName . termFOut

instance Apply Declarations1 fs => Declarations1 (Sum fs) where
  liftDeclaredName f = apply @Declarations1 (liftDeclaredName f)

instance Declarations1 []
