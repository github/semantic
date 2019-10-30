{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Analysis.PackageDef
( PackageDef(..)
, HasPackageDef
, packageDefAlgebra
) where

import Data.Blob
import Source.Source as Source
import Data.Sum
import Data.Term
import qualified Data.Text as T
import qualified Language.Go.Syntax
import Prologue
import Source.Loc

newtype PackageDef = PackageDef { moduleDefIdentifier :: T.Text }
  deriving (Eq, Show)

-- | An r-algebra producing 'Just' a 'PackageDef' for syntax nodes corresponding to high-level declarations, or 'Nothing' otherwise.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a 'CustomHasPackageDef' instance for the type.
--   2. Adding the type to the 'PackageDefStrategy' type family.
--
--   If you’re getting errors about missing a 'CustomHasPackageDef' instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
packageDefAlgebra :: (Foldable syntax, HasPackageDef syntax) => Blob -> RAlgebra (TermF syntax Loc) (Term syntax Loc) (Maybe PackageDef)
packageDefAlgebra blob (In ann syntax) = toPackageDef blob ann syntax


-- | Types for which we can produce a 'PackageDef' in 'Maybe'. There is exactly one instance of this typeclass; adding customized 'PackageDef's for a new type is done by defining an instance of 'CustomHasPackageDef' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasPackageDef syntax where
  -- | Compute a 'PackageDef' for a syntax type using its 'CustomHasPackageDef' instance, if any, or else falling back to the default definition (which simply returns 'Nothing').
  toPackageDef :: (Foldable whole) => Blob -> Loc -> syntax (Term whole Loc, Maybe PackageDef) -> Maybe PackageDef

-- | Define 'toPackageDef' using the 'CustomHasPackageDef' instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'PackageDefStrategy' type family. Thus producing a 'PackageDef' for a node requires both defining a 'CustomHasPackageDef' instance _and_ adding a definition for the type to the 'PackageDefStrategy' type family to return 'Custom'.
--
--   Note that since 'PackageDefStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasPackageDef', as any other instance would be indistinguishable.
instance (PackageDefStrategy syntax ~ strategy, HasPackageDefWithStrategy strategy syntax) => HasPackageDef syntax where
  toPackageDef = toPackageDefWithStrategy (Proxy :: Proxy strategy)


-- | Types for which we can produce a customized 'PackageDef'. This returns in 'Maybe' so that some values can be opted out (e.g. anonymous functions).
class CustomHasPackageDef syntax where
  -- | Produce a customized 'PackageDef' for a given syntax node.
  customToPackageDef :: (Foldable whole) => Blob -> Loc -> syntax (Term whole Loc, Maybe PackageDef) -> Maybe PackageDef


instance CustomHasPackageDef Language.Go.Syntax.Package where
  customToPackageDef Blob{..} _ (Language.Go.Syntax.Package (Term (In fromAnn _), _) _)
    = Just $ PackageDef (getSource fromAnn)
    where getSource = toText . Source.slice blobSource . byteRange

-- | Produce a 'PackageDef' for 'Sum's using the 'HasPackageDef' instance & therefore using a 'CustomHasPackageDef' instance when one exists & the type is listed in 'PackageDefStrategy'.
instance Apply HasPackageDef fs => CustomHasPackageDef (Sum fs) where
  customToPackageDef blob ann = apply @HasPackageDef (toPackageDef blob ann)


-- | A strategy for defining a 'HasPackageDef' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | Produce a 'PackageDef' for a syntax node using either the 'Default' or 'Custom' strategy.
--
--   You should probably be using 'CustomHasPackageDef' instead of this class; and you should not define new instances of this class.
class HasPackageDefWithStrategy (strategy :: Strategy) syntax where
  toPackageDefWithStrategy :: (Foldable whole) => proxy strategy -> Blob -> Loc -> syntax (Term whole Loc, Maybe PackageDef) -> Maybe PackageDef


-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a 'CustomHasPackageDef' instance for a given type, you’ve probably listed it in here but not defined a 'CustomHasPackageDef' instance for it, or else you’ve listed the wrong type in here. Conversely, if your 'customHasPackageDef' method is never being called, you may have forgotten to list the type in here.
type family PackageDefStrategy syntax where
  PackageDefStrategy Language.Go.Syntax.Package = 'Custom
  PackageDefStrategy (Sum _) = 'Custom
  PackageDefStrategy _ = 'Default


-- | The 'Default' strategy produces 'Nothing'.
instance HasPackageDefWithStrategy 'Default syntax where
  toPackageDefWithStrategy _ _ _ _ = Nothing

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomHasPackageDef' instance for the type.
instance CustomHasPackageDef syntax => HasPackageDefWithStrategy 'Custom syntax where
  toPackageDefWithStrategy _ = customToPackageDef
