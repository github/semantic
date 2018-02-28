{-# LANGUAGE DataKinds, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Analysis.ModuleDef
( ModuleDef(..)
, HasModuleDef
, moduleDefAlgebra
) where

import Prologue
import Data.Blob
import Data.Range
import Data.Record
import Data.Source as Source
import Data.Span
import qualified Data.Syntax.Declaration as Declaration
import Data.Term
import qualified Data.Text as T

newtype ModuleDef = ModuleDef { moduleDefIdentifier :: T.Text }
  deriving (Eq, Generic, Show)

-- | An r-algebra producing 'Just' a 'ModuleDef' for syntax nodes corresponding to high-level declarations, or 'Nothing' otherwise.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a 'CustomHasModuleDef' instance for the type.
--   2. Adding the type to the 'ModuleDefStrategy' type family.
--
--   If you’re getting errors about missing a 'CustomHasModuleDef' instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
moduleDefAlgebra :: (HasField fields Range, HasField fields Span, Foldable syntax, HasModuleDef syntax) => Blob -> RAlgebra (TermF syntax (Record fields)) (Term syntax (Record fields)) (Maybe ModuleDef)
moduleDefAlgebra blob (In ann syntax) = toModuleDef blob ann syntax


-- | Types for which we can produce a 'ModuleDef' in 'Maybe'. There is exactly one instance of this typeclass; adding customized 'ModuleDef's for a new type is done by defining an instance of 'CustomHasModuleDef' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasModuleDef syntax where
  -- | Compute a 'ModuleDef' for a syntax type using its 'CustomHasModuleDef' instance, if any, or else falling back to the default definition (which simply returns 'Nothing').
  toModuleDef :: (Foldable whole, HasField fields Range, HasField fields Span) => Blob -> Record fields -> syntax (Term whole (Record fields), Maybe ModuleDef) -> Maybe ModuleDef

-- | Define 'toModuleDef' using the 'CustomHasModuleDef' instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'ModuleDefStrategy' type family. Thus producing a 'ModuleDef' for a node requires both defining a 'CustomHasModuleDef' instance _and_ adding a definition for the type to the 'ModuleDefStrategy' type family to return 'Custom'.
--
--   Note that since 'ModuleDefStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasModuleDef', as any other instance would be indistinguishable.
instance (ModuleDefStrategy syntax ~ strategy, HasModuleDefWithStrategy strategy syntax) => HasModuleDef syntax where
  toModuleDef = toModuleDefWithStrategy (Proxy :: Proxy strategy)


-- | Types for which we can produce a customized 'ModuleDef'. This returns in 'Maybe' so that some values can be opted out (e.g. anonymous functions).
class CustomHasModuleDef syntax where
  -- | Produce a customized 'ModuleDef' for a given syntax node.
  customToModuleDef :: (Foldable whole, HasField fields Range, HasField fields Span) => Blob -> Record fields -> syntax (Term whole (Record fields), Maybe ModuleDef) -> Maybe ModuleDef


instance CustomHasModuleDef Declaration.Module where
  customToModuleDef Blob{..} _ (Declaration.Module (Term (In fromAnn _), _) _)
    = Just $ ModuleDef (getSource fromAnn)
    where getSource = toText . flip Source.slice blobSource . getField

-- | Produce a 'ModuleDef' for 'Union's using the 'HasModuleDef' instance & therefore using a 'CustomHasModuleDef' instance when one exists & the type is listed in 'ModuleDefStrategy'.
instance Apply HasModuleDef fs => CustomHasModuleDef (Union fs) where
  customToModuleDef blob ann = apply (Proxy :: Proxy HasModuleDef) (toModuleDef blob ann)


-- | A strategy for defining a 'HasModuleDef' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | Produce a 'ModuleDef' for a syntax node using either the 'Default' or 'Custom' strategy.
--
--   You should probably be using 'CustomHasModuleDef' instead of this class; and you should not define new instances of this class.
class HasModuleDefWithStrategy (strategy :: Strategy) syntax where
  toModuleDefWithStrategy :: (Foldable whole, HasField fields Range, HasField fields Span) => proxy strategy -> Blob -> Record fields -> syntax (Term whole (Record fields), Maybe ModuleDef) -> Maybe ModuleDef


-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a 'CustomHasModuleDef' instance for a given type, you’ve probably listed it in here but not defined a 'CustomHasModuleDef' instance for it, or else you’ve listed the wrong type in here. Conversely, if your 'customHasModuleDef' method is never being called, you may have forgotten to list the type in here.
type family ModuleDefStrategy syntax where
  ModuleDefStrategy Declaration.Module = 'Custom
  ModuleDefStrategy (Union fs) = 'Custom
  ModuleDefStrategy a = 'Default


-- | The 'Default' strategy produces 'Nothing'.
instance HasModuleDefWithStrategy 'Default syntax where
  toModuleDefWithStrategy _ _ _ _ = Nothing

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomHasModuleDef' instance for the type.
instance CustomHasModuleDef syntax => HasModuleDefWithStrategy 'Custom syntax where
  toModuleDefWithStrategy _ = customToModuleDef
