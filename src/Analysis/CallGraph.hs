{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Analysis.CallGraph
( CallGraph
, renderCallGraph
, buildCallGraph
, CallGraphAlgebra(..)
) where

import Algebra.Graph.Export.Dot
import Data.Abstract.FreeVariables
import Data.Graph
import Data.Semilattice.Lower
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import Data.Term
import Prologue

type CallGraph = Graph Name

-- | Build the 'CallGraph' for a 'Term' recursively.
buildCallGraph :: (CallGraphAlgebra syntax, FreeVariables1 syntax, Functor syntax) => Term syntax ann -> [Name] -> CallGraph
buildCallGraph = foldSubterms callGraphAlgebra


-- | Render a 'CallGraph' to a 'ByteString' in DOT notation.
renderCallGraph :: CallGraph -> ByteString
renderCallGraph = export (defaultStyle unName)


-- | Types which contribute to a 'CallGraph'. There is exactly one instance of this typeclass; customizing the 'CallGraph's for a new type is done by defining an instance of 'CustomCallGraphAlgebra' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class CallGraphAlgebra syntax where
  -- | A 'SubtermAlgebra' computing the 'CallGraph' for a piece of @syntax@.
  callGraphAlgebra :: FreeVariables term => syntax (Subterm term ([Name] -> CallGraph)) -> [Name] -> CallGraph

instance (CallGraphAlgebraStrategy syntax ~ strategy, CallGraphAlgebraWithStrategy strategy syntax) => CallGraphAlgebra syntax where
  callGraphAlgebra = callGraphAlgebraWithStrategy (Proxy :: Proxy strategy)


-- | Types whose contribution to a 'CallGraph' is customized. If an instance’s definition is not being used, ensure that the type is mapped to 'Custom' in the 'CallGraphAlgebraStrategy'.
class CustomCallGraphAlgebra syntax where
  customCallGraphAlgebra :: FreeVariables term => syntax (Subterm term ([Name] -> CallGraph)) -> [Name] -> CallGraph

-- | 'Declaration.Function's produce a vertex for their name, with edges to any free variables in their body.
instance CustomCallGraphAlgebra Declaration.Function where
  customCallGraphAlgebra Declaration.Function{..} bound = foldMap vertex (freeVariables (subterm functionName)) `connect` subtermValue functionBody (foldMap (freeVariables . subterm) functionParameters <> bound)

-- | 'Declaration.Method's produce a vertex for their name, with edges to any free variables in their body.
instance CustomCallGraphAlgebra Declaration.Method where
  customCallGraphAlgebra Declaration.Method{..} bound = foldMap vertex (freeVariables (subterm methodName)) `connect` subtermValue methodBody (foldMap (freeVariables . subterm) methodParameters <> bound)

-- | 'Syntax.Identifier's produce a vertex iff it’s unbound in the 'Set'.
instance CustomCallGraphAlgebra Syntax.Identifier where
  customCallGraphAlgebra (Syntax.Identifier name) bound
    | name `elem` bound = lowerBound
    | otherwise         = vertex name

instance Apply CallGraphAlgebra syntaxes => CustomCallGraphAlgebra (Sum syntaxes) where
  customCallGraphAlgebra = apply @CallGraphAlgebra callGraphAlgebra

instance CallGraphAlgebra syntax => CustomCallGraphAlgebra (TermF syntax a) where
  customCallGraphAlgebra = callGraphAlgebra . termFOut


-- | The mechanism selecting 'Default'/'Custom' implementations for 'callGraphAlgebra' depending on the @syntax@ type.
class CallGraphAlgebraWithStrategy (strategy :: Strategy) syntax where
  callGraphAlgebraWithStrategy :: FreeVariables term => proxy strategy -> syntax (Subterm term ([Name] -> CallGraph)) -> [Name] -> CallGraph

-- | The 'Default' definition of 'callGraphAlgebra' combines all of the 'CallGraph's within the @syntax@ 'Monoid'ally.
instance Foldable syntax => CallGraphAlgebraWithStrategy 'Default syntax where
  callGraphAlgebraWithStrategy _ = foldMap subtermValue

-- | The 'Custom' strategy calls out to the 'customCallGraphAlgebra' method.
instance CustomCallGraphAlgebra syntax => CallGraphAlgebraWithStrategy 'Custom syntax where
  callGraphAlgebraWithStrategy _ = customCallGraphAlgebra


-- | Which instance of 'CustomCallGraphAlgebra' to use for a given @syntax@ type.
data Strategy = Default | Custom

-- | A mapping of @syntax@ types onto 'Strategy's.
type family CallGraphAlgebraStrategy syntax where
  CallGraphAlgebraStrategy Declaration.Function = 'Custom
  CallGraphAlgebraStrategy Declaration.Method = 'Custom
  CallGraphAlgebraStrategy Syntax.Identifier = 'Custom
  CallGraphAlgebraStrategy (Sum fs) = 'Custom
  CallGraphAlgebraStrategy (TermF f a) = 'Custom
  CallGraphAlgebraStrategy a = 'Default
