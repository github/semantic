{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.ImportGraph
( ImportGraph(..)
, renderImportGraph
, ImportGraphing
) where

import qualified Algebra.Graph as G
import Algebra.Graph.Class
import Algebra.Graph.Export.Dot
import Control.Abstract.Analysis
import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Abstract.Evaluatable (LoadError(..))
import Prologue hiding (empty)

-- | The graph of function definitions to symbols used in a given program.
newtype ImportGraph = ImportGraph { unImportGraph :: G.Graph Name }
  deriving (Eq, Graph, Show)

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


instance ( Effectful (m term value)
         , Member (State ImportGraph) effects
         , MonadAnalysis term value (m term value effects)
         , Member (Resumable (LoadError term value)) effects
         )
         => MonadAnalysis term value (ImportGraphing m term value effects) where
  type RequiredEffects term value (ImportGraphing m term value effects) = State ImportGraph ': RequiredEffects term value (m term value effects)

  analyzeTerm eval term = resumeException @(LoadError term value) (liftAnalyze analyzeTerm eval term) (\yield (LoadError name) ->
    do
      ms <- askModuleStack
      let parent = maybe empty (vertex . moduleName) (listToMaybe ms)
      modifyImportGraph (parent >< vertex name <>)
      yield []
    )

  analyzeModule recur m = do
    ms <- askModuleStack
    let parent = maybe empty (vertex . moduleName) (listToMaybe ms)
    modifyImportGraph (parent >< vertex (moduleName m) <>)
    liftAnalyze analyzeModule recur m

(><) :: Graph a => a -> a -> a
(><) = connect

infixr 7 ><

modifyImportGraph :: (Effectful (m term value), Member (State ImportGraph) effects) => (ImportGraph -> ImportGraph) -> ImportGraphing m term value effects ()
modifyImportGraph = raise . modify


instance Semigroup ImportGraph where
  (<>) = overlay

instance Monoid ImportGraph where
  mempty = empty
  mappend = (<>)

instance Ord ImportGraph where
  compare (ImportGraph G.Empty)           (ImportGraph G.Empty)           = EQ
  compare (ImportGraph G.Empty)           _                               = LT
  compare _                               (ImportGraph G.Empty)           = GT
  compare (ImportGraph (G.Vertex a))      (ImportGraph (G.Vertex b))      = compare a b
  compare (ImportGraph (G.Vertex _))      _                               = LT
  compare _                               (ImportGraph (G.Vertex _))      = GT
  compare (ImportGraph (G.Overlay a1 a2)) (ImportGraph (G.Overlay b1 b2)) = (compare `on` ImportGraph) a1 b1 <> (compare `on` ImportGraph) a2 b2
  compare (ImportGraph (G.Overlay _  _))  _                               = LT
  compare _                               (ImportGraph (G.Overlay _ _))   = GT
  compare (ImportGraph (G.Connect a1 a2)) (ImportGraph (G.Connect b1 b2)) = (compare `on` ImportGraph) a1 b1 <> (compare `on` ImportGraph) a2 b2
