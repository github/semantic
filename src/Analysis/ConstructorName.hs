{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.ConstructorName
( ConstructorName(..)
) where

import Data.Sum
import Data.Term
import GHC.Generics
import Prologue

-- | A typeclass to retrieve the name of the data constructor for a value.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap; see also src/Analysis/Declaration.hs for discussion of the details of the mechanism.
class ConstructorName syntax where
  constructorName :: syntax a -> String

instance (ConstructorNameStrategy syntax ~ strategy, ConstructorNameWithStrategy strategy syntax) => ConstructorName syntax where
  constructorName = constructorNameWithStrategy (Proxy :: Proxy strategy)

instance Apply ConstructorName fs => ConstructorNameWithStrategy 'Custom (Sum fs) where
  constructorNameWithStrategy _ = apply @ConstructorName constructorName

instance ConstructorNameWithStrategy 'Custom [] where
  constructorNameWithStrategy _ _ = "Statements"

instance (ConstructorName syntax) => ConstructorNameWithStrategy 'Custom (TermF syntax ann) where
  constructorNameWithStrategy _ = constructorName . termFOut

data Strategy = Default | Custom

type family ConstructorNameStrategy syntax where
  ConstructorNameStrategy (Sum _)     = 'Custom
  ConstructorNameStrategy []          = 'Custom
  ConstructorNameStrategy (TermF _ _) = 'Custom
  ConstructorNameStrategy _           = 'Default

class ConstructorNameWithStrategy (strategy :: Strategy) syntax where
  constructorNameWithStrategy :: proxy strategy -> syntax a -> String

instance (Generic1 syntax, GConstructorName (Rep1 syntax)) => ConstructorNameWithStrategy 'Default syntax where
  constructorNameWithStrategy _ = gconstructorName . from1


class GConstructorName f where
  gconstructorName :: f a -> String

instance GConstructorName f => GConstructorName (M1 D c f) where
  gconstructorName = gconstructorName . unM1

instance (GConstructorName f, GConstructorName g) => GConstructorName (f :+: g) where
  gconstructorName (L1 l) = gconstructorName l
  gconstructorName (R1 r) = gconstructorName r

instance Constructor c => GConstructorName (M1 C c f) where
  gconstructorName = conName
