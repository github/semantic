{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.ImportGraph
( ImportGraph(..)
, ImportGraphing
, renderImportGraph
, buildImportGraph
, ImportGraphAlgebra(..)
) where

import qualified Algebra.Graph as G
import Algebra.Graph.Class
import Algebra.Graph.Export.Dot
import Control.Abstract.Analysis
import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Set (member)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import Data.Term
import Prologue hiding (empty)

-- | The graph of function definitions to symbols used in a given program.
newtype ImportGraph = ImportGraph { unImportGraph :: G.Graph Name }
  deriving (Eq, Graph, Show)

-- | Build the 'ImportGraph' for a 'Term' recursively.
buildImportGraph :: (ImportGraphAlgebra syntax, FreeVariables1 syntax, Functor syntax) => Term syntax ann -> Set Name -> ImportGraph
buildImportGraph = foldSubterms importGraphAlgebra


-- | Render a 'ImportGraph' to a 'ByteString' in DOT notation.
renderImportGraph :: ImportGraph -> ByteString
renderImportGraph = export (defaultStyle friendlyName) . unImportGraph

newtype ImportGraphing m term value (effects :: [* -> *]) a = ImportGraphing (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (ImportGraphing m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (ImportGraphing m term value effects)
deriving instance MonadHeap value (m term value effects) => MonadHeap value (ImportGraphing m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (ImportGraphing m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (ImportGraphing m term value effects)


instance MonadAnalysis term value (m term value effects) => MonadAnalysis term value (ImportGraphing m term value effects) where
  type RequiredEffects term value (ImportGraphing m term value effects) = RequiredEffects term value (m term value effects)

  analyzeTerm = liftAnalyze analyzeTerm

  evaluateModule m@Module{..} = ImportGraphing (evaluateModule m)


-- | Types which contribute to a 'ImportGraph'. There is exactly one instance of this typeclass; customizing the 'ImportGraph's for a new type is done by defining an instance of 'CustomImportGraphAlgebra' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class ImportGraphAlgebra syntax where
  -- | A 'SubtermAlgebra' computing the 'ImportGraph' for a piece of @syntax@.
  importGraphAlgebra :: FreeVariables term => syntax (Subterm term (Set Name -> ImportGraph)) -> Set Name -> ImportGraph

instance (ImportGraphAlgebraStrategy syntax ~ strategy, ImportGraphAlgebraWithStrategy strategy syntax) => ImportGraphAlgebra syntax where
  importGraphAlgebra = importGraphAlgebraWithStrategy (Proxy :: Proxy strategy)


-- | Types whose contribution to a 'ImportGraph' is customized. If an instance’s definition is not being used, ensure that the type is mapped to 'Custom' in the 'ImportGraphAlgebraStrategy'.
class CustomImportGraphAlgebra syntax where
  customImportGraphAlgebra :: FreeVariables term => syntax (Subterm term (Set Name -> ImportGraph)) -> Set Name -> ImportGraph

-- | 'Declaration.Function's produce a vertex for their name, with edges to any free variables in their body.
instance CustomImportGraphAlgebra Declaration.Function where
  customImportGraphAlgebra Declaration.Function{..} bound = foldMap vertex (freeVariables (subterm functionName)) `connect` subtermValue functionBody (foldMap (freeVariables . subterm) functionParameters <> bound)

-- | 'Declaration.Method's produce a vertex for their name, with edges to any free variables in their body.
instance CustomImportGraphAlgebra Declaration.Method where
  customImportGraphAlgebra Declaration.Method{..} bound = foldMap vertex (freeVariables (subterm methodName)) `connect` subtermValue methodBody (foldMap (freeVariables . subterm) methodParameters <> bound)

-- | 'Syntax.Identifier's produce a vertex iff it’s unbound in the 'Set'.
instance CustomImportGraphAlgebra Syntax.Identifier where
  customImportGraphAlgebra (Syntax.Identifier name) bound
    | name `member` bound = empty
    | otherwise           = vertex name

instance Apply ImportGraphAlgebra syntaxes => CustomImportGraphAlgebra (Union syntaxes) where
  customImportGraphAlgebra = Prologue.apply (Proxy :: Proxy ImportGraphAlgebra) importGraphAlgebra

instance ImportGraphAlgebra syntax => CustomImportGraphAlgebra (TermF syntax a) where
  customImportGraphAlgebra = importGraphAlgebra . termFOut


-- | The mechanism selecting 'Default'/'Custom' implementations for 'importGraphAlgebra' depending on the @syntax@ type.
class ImportGraphAlgebraWithStrategy (strategy :: Strategy) syntax where
  importGraphAlgebraWithStrategy :: FreeVariables term => proxy strategy -> syntax (Subterm term (Set Name -> ImportGraph)) -> Set Name -> ImportGraph

-- | The 'Default' definition of 'importGraphAlgebra' combines all of the 'ImportGraph's within the @syntax@ 'Monoid'ally.
instance Foldable syntax => ImportGraphAlgebraWithStrategy 'Default syntax where
  importGraphAlgebraWithStrategy _ = foldMap subtermValue

-- | The 'Custom' strategy calls out to the 'customImportGraphAlgebra' method.
instance CustomImportGraphAlgebra syntax => ImportGraphAlgebraWithStrategy 'Custom syntax where
  importGraphAlgebraWithStrategy _ = customImportGraphAlgebra


-- | Which instance of 'CustomImportGraphAlgebra' to use for a given @syntax@ type.
data Strategy = Default | Custom

-- | A mapping of @syntax@ types onto 'Strategy's.
type family ImportGraphAlgebraStrategy syntax where
  ImportGraphAlgebraStrategy Declaration.Function = 'Custom
  ImportGraphAlgebraStrategy Declaration.Method = 'Custom
  ImportGraphAlgebraStrategy Syntax.Identifier = 'Custom
  ImportGraphAlgebraStrategy (Union fs) = 'Custom
  ImportGraphAlgebraStrategy (TermF f a) = 'Custom
  ImportGraphAlgebraStrategy a = 'Default

instance Semigroup ImportGraph where
  (<>) = overlay

instance Monoid ImportGraph where
  mempty = empty
  mappend = (<>)

instance Ord ImportGraph where
  compare (ImportGraph G.Empty)           (ImportGraph G.Empty)           = EQ
  compare (ImportGraph G.Empty)           _                             = LT
  compare _                             (ImportGraph G.Empty)           = GT
  compare (ImportGraph (G.Vertex a))      (ImportGraph (G.Vertex b))      = compare a b
  compare (ImportGraph (G.Vertex _))      _                             = LT
  compare _                             (ImportGraph (G.Vertex _))      = GT
  compare (ImportGraph (G.Overlay a1 a2)) (ImportGraph (G.Overlay b1 b2)) = (compare `on` ImportGraph) a1 b1 <> (compare `on` ImportGraph) a2 b2
  compare (ImportGraph (G.Overlay _  _))  _                             = LT
  compare _                             (ImportGraph (G.Overlay _ _))   = GT
  compare (ImportGraph (G.Connect a1 a2)) (ImportGraph (G.Connect b1 b2)) = (compare `on` ImportGraph) a1 b1 <> (compare `on` ImportGraph) a2 b2
