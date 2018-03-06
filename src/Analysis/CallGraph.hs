{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Analysis.CallGraph where

import qualified Algebra.Graph as G
import Algebra.Graph.Class
import Algebra.Graph.Export.Dot
import Data.Abstract.FreeVariables
import Data.Set (member)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import Data.Term
import Prologue hiding (empty)

newtype CallGraph = CallGraph { unCallGraph :: G.Graph Name }
  deriving (Eq, Graph, Show)

renderCallGraph :: CallGraph -> ByteString
renderCallGraph = export (defaultStyle id) . unCallGraph


buildCallGraph :: (IsDeclaration syntax, Foldable syntax, FreeVariables1 syntax, Functor syntax) => Term syntax ann -> Set Name -> CallGraph
buildCallGraph = foldSubterms buildCallGraphAlgebra


class IsDeclaration syntax where
  buildCallGraphAlgebra :: FreeVariables term => syntax (Subterm term (Set Name -> CallGraph)) -> Set Name -> CallGraph

instance (IsDeclarationStrategy syntax ~ strategy, IsDeclarationWithStrategy strategy syntax) => IsDeclaration syntax where
  buildCallGraphAlgebra = buildCallGraphAlgebraWithStrategy (Proxy :: Proxy strategy)

class CustomIsDeclaration syntax where
  customBuildCallGraphAlgebra :: FreeVariables term => syntax (Subterm term (Set Name -> CallGraph)) -> Set Name -> CallGraph

instance CustomIsDeclaration Declaration.Function where
  customBuildCallGraphAlgebra Declaration.Function{..} bound = foldMap vertex (freeVariables (subterm functionName)) `connect` subtermValue functionBody (foldMap (freeVariables . subterm) functionParameters <> bound)

instance CustomIsDeclaration Declaration.Method where
  customBuildCallGraphAlgebra Declaration.Method{..} bound = foldMap vertex (freeVariables (subterm methodName)) `connect` subtermValue methodBody (foldMap (freeVariables . subterm) methodParameters <> bound)

instance CustomIsDeclaration Syntax.Identifier where
  customBuildCallGraphAlgebra (Syntax.Identifier name) bound
    | name `member` bound = empty
    | otherwise           = vertex name

instance Apply IsDeclaration syntaxes => CustomIsDeclaration (Union syntaxes) where
  customBuildCallGraphAlgebra = Prologue.apply (Proxy :: Proxy IsDeclaration) buildCallGraphAlgebra

instance IsDeclaration syntax => CustomIsDeclaration (TermF syntax a) where
  customBuildCallGraphAlgebra = buildCallGraphAlgebra . termFOut

class IsDeclarationWithStrategy (strategy :: Strategy) syntax where
  buildCallGraphAlgebraWithStrategy :: FreeVariables term => proxy strategy -> syntax (Subterm term (Set Name -> CallGraph)) -> Set Name -> CallGraph

instance Foldable syntax => IsDeclarationWithStrategy 'Default syntax where
  buildCallGraphAlgebraWithStrategy _ = foldMap subtermValue

instance CustomIsDeclaration syntax => IsDeclarationWithStrategy 'Custom syntax where
  buildCallGraphAlgebraWithStrategy _ = customBuildCallGraphAlgebra

data Strategy = Default | Custom

type family IsDeclarationStrategy syntax where
  IsDeclarationStrategy Declaration.Function = 'Custom
  IsDeclarationStrategy Declaration.Method = 'Custom
  IsDeclarationStrategy Syntax.Identifier = 'Custom
  IsDeclarationStrategy (Union fs) = 'Custom
  IsDeclarationStrategy (TermF f a) = 'Custom
  IsDeclarationStrategy a = 'Default


instance Monoid CallGraph where
  mempty = empty
  mappend = overlay

instance Ord CallGraph where
  compare (CallGraph G.Empty)           (CallGraph G.Empty)           = EQ
  compare (CallGraph G.Empty)           _                             = LT
  compare _                             (CallGraph G.Empty)           = GT
  compare (CallGraph (G.Vertex a))      (CallGraph (G.Vertex b))      = compare a b
  compare (CallGraph (G.Vertex _))      _                             = LT
  compare _                             (CallGraph (G.Vertex _))      = GT
  compare (CallGraph (G.Overlay a1 a2)) (CallGraph (G.Overlay b1 b2)) = (compare `on` CallGraph) a1 b1 <> (compare `on` CallGraph) a2 b2
  compare (CallGraph (G.Overlay _  _))  _                             = LT
  compare _                             (CallGraph (G.Overlay _ _))   = GT
  compare (CallGraph (G.Connect a1 a2)) (CallGraph (G.Connect b1 b2)) = (compare `on` CallGraph) a1 b1 <> (compare `on` CallGraph) a2 b2
