{-# LANGUAGE DataKinds, DefaultSignatures, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Analysis.CyclomaticComplexity
( CyclomaticComplexity(..)
, HasCyclomaticComplexity
, cyclomaticComplexityAlgebra
) where

import Data.Aeson
import Data.Sum
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Term
import Prologue

-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show, ToJSON)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods or functions should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.

-- | An f-algebra producing a 'CyclomaticComplexity' for syntax nodes corresponding to their summary cyclomatic complexity, defaulting to the sum of their contents’ cyclomatic complexities.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a 'CustomHasCyclomaticComplexity' instance for the type.
--   2. Adding the type to the 'CyclomaticComplexityStrategy' type family.
--
--   If you’re getting errors about missing a 'CustomHasCyclomaticComplexity' instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
cyclomaticComplexityAlgebra :: HasCyclomaticComplexity syntax => TermF syntax ann CyclomaticComplexity -> CyclomaticComplexity
cyclomaticComplexityAlgebra (In _ syntax) = toCyclomaticComplexity syntax


-- | Types for which we can produce a 'CyclomaticComplexity'. There is exactly one instance of this typeclass; adding customized 'CyclomaticComplexity's for a new type is done by defining an instance of 'CustomHasCyclomaticComplexity' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasCyclomaticComplexity syntax where
  -- | Compute a 'CyclomaticComplexity' for a syntax type using its 'CustomHasCyclomaticComplexity' instance, if any, or else falling back to the default definition (which simply returns the sum of any contained cyclomatic complexities).
  toCyclomaticComplexity :: syntax CyclomaticComplexity -> CyclomaticComplexity

-- | Define 'toCyclomaticComplexity' using the 'CustomHasCyclomaticComplexity' instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'CyclomaticComplexityStrategy' type family. Thus producing a 'CyclomaticComplexity' for a node requires both defining a 'CustomHasCyclomaticComplexity' instance _and_ adding a definition for the type to the 'CyclomaticComplexityStrategy' type family to return 'Custom'.
--
--   Note that since 'CyclomaticComplexityStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasCyclomaticComplexity', as any other instance would be indistinguishable.
instance (CyclomaticComplexityStrategy syntax ~ strategy, HasCyclomaticComplexityWithStrategy strategy syntax) => HasCyclomaticComplexity syntax where
  toCyclomaticComplexity = toCyclomaticComplexityWithStrategy (Proxy :: Proxy strategy)


-- | Types for which we can produce a customized 'CyclomaticComplexity'.
class CustomHasCyclomaticComplexity syntax where
  -- | Produce a customized 'CyclomaticComplexity' for a given syntax node.
  customToCyclomaticComplexity :: syntax CyclomaticComplexity -> CyclomaticComplexity

  -- | Because we perform the same operation wherever we use the custom strategy, we can define the default method for all instances.
  default customToCyclomaticComplexity :: Foldable syntax => syntax CyclomaticComplexity -> CyclomaticComplexity
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Declaration.Function
instance CustomHasCyclomaticComplexity Declaration.Method
instance CustomHasCyclomaticComplexity Statement.Catch
instance CustomHasCyclomaticComplexity Statement.DoWhile
instance CustomHasCyclomaticComplexity Statement.Else
instance CustomHasCyclomaticComplexity Statement.For
instance CustomHasCyclomaticComplexity Statement.ForEach
instance CustomHasCyclomaticComplexity Statement.If
instance CustomHasCyclomaticComplexity Statement.Pattern
instance CustomHasCyclomaticComplexity Statement.While

-- | Produce a 'CyclomaticComplexity' for 'Sum's using the 'HasCyclomaticComplexity' instance & therefore using a 'CustomHasCyclomaticComplexity' instance when one exists & the type is listed in 'CyclomaticComplexityStrategy'.
instance Apply HasCyclomaticComplexity fs => CustomHasCyclomaticComplexity (Sum fs) where
  customToCyclomaticComplexity = apply @HasCyclomaticComplexity toCyclomaticComplexity


-- | A strategy for defining a 'HasCyclomaticComplexity' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | Produce a 'CyclomaticComplexity' for a syntax node using either the 'Default' or 'Custom' strategy.
--
--   You should probably be using 'CustomHasCyclomaticComplexity' instead of this class; and you should not define new instances of this class.
class HasCyclomaticComplexityWithStrategy (strategy :: Strategy) syntax where
  toCyclomaticComplexityWithStrategy :: proxy strategy -> syntax CyclomaticComplexity -> CyclomaticComplexity


-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a 'CustomHasCyclomaticComplexity' instance for a given type, you’ve probably listed it in here but not defined a 'CustomHasCyclomaticComplexity' instance for it, or else you’ve listed the wrong type in here. Conversely, if your 'customHasCyclomaticComplexity' method is never being called, you may have forgotten to list the type in here.
type family CyclomaticComplexityStrategy syntax where
  CyclomaticComplexityStrategy Declaration.Function = 'Custom
  CyclomaticComplexityStrategy Declaration.Method = 'Custom
  CyclomaticComplexityStrategy Statement.Catch = 'Custom
  CyclomaticComplexityStrategy Statement.DoWhile = 'Custom
  CyclomaticComplexityStrategy Statement.Else = 'Custom
  CyclomaticComplexityStrategy Statement.For = 'Custom
  CyclomaticComplexityStrategy Statement.ForEach = 'Custom
  CyclomaticComplexityStrategy Statement.If = 'Custom
  CyclomaticComplexityStrategy Statement.Pattern = 'Custom
  CyclomaticComplexityStrategy Statement.While = 'Custom
  CyclomaticComplexityStrategy (Sum _) = 'Custom
  CyclomaticComplexityStrategy _ = 'Default


-- | The 'Default' strategy takes the sum without incrementing.
instance Foldable syntax => HasCyclomaticComplexityWithStrategy 'Default syntax where
  toCyclomaticComplexityWithStrategy _ = sum

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomHasCyclomaticComplexity' instance for the type.
instance CustomHasCyclomaticComplexity syntax => HasCyclomaticComplexityWithStrategy 'Custom syntax where
  toCyclomaticComplexityWithStrategy _ = customToCyclomaticComplexity
