{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeApplications, UndecidableInstances #-}
module Data.Abstract.Declarations
  ( Declarations (..)
  , Declarations1 (..)
  ) where

import Data.Abstract.Name
import Data.Sum
import Data.Term

class Declarations syntax where
  declaredName :: syntax -> Maybe Name
  declaredName = const Nothing

  declaredAlias :: syntax -> Maybe Name
  declaredAlias = const Nothing

class Declarations1 syntax where
  -- | Lift a function mapping each element to its declared name (if any) through a containing structure. This can be used to define the declared name for a composite piece of syntax in terms of the declared name of one of its components.
  --
  -- Note that not all syntax will have a declared name; in general it’s reserved for syntax where the user has provided a single, unambiguous name for whatever term is being introduced. Examples would be (non-anonymous) functions, methods, and classes; but not (generally) literals or blocks of imperative statements.
  liftDeclaredName :: (a -> Maybe Name) -> syntax a -> Maybe Name
  liftDeclaredName _ _ = Nothing

  liftDeclaredAlias :: (a -> Maybe Name) -> syntax a -> Maybe Name
  liftDeclaredAlias _ _ = Nothing

deriving instance Declarations1 syntax => Declarations (Term syntax ann)

instance (Declarations recur, Declarations1 syntax) => Declarations (TermF syntax ann recur) where
  declaredName = liftDeclaredName declaredName . termFOut
  declaredAlias = liftDeclaredAlias declaredAlias . termFOut

instance Apply Declarations1 fs => Declarations1 (Sum fs) where
  liftDeclaredName f = apply @Declarations1 (liftDeclaredName f)
  liftDeclaredAlias f = apply @Declarations1 (liftDeclaredAlias f)

instance Declarations1 []
