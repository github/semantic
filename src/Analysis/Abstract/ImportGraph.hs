{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.ImportGraph
( ImportGraph(..)
, renderImportGraph
, ImportGraphing
) where

import qualified Algebra.Graph as G
import           Algebra.Graph.Class hiding (Vertex)
import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract.Analysis
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable (LoadError (..))
import           Data.Abstract.FreeVariables
import           Data.Abstract.Located
import           Data.Abstract.Module hiding (Module)
import           Data.Abstract.Origin hiding (Module, Package)
import           Data.Abstract.Package hiding (Package)
import qualified Data.Syntax as Syntax
import           Data.Term
import           Prologue hiding (empty, packageName)

-- | The graph of function definitions to symbols used in a given program.
newtype ImportGraph = ImportGraph { unImportGraph :: G.Graph Vertex }
  deriving (Eq, Graph, Show)

-- | A vertex of some specific type.
data Vertex
  = Package  { vertexName :: PackageName }
  | Module   { vertexName :: ModuleName }
  | Variable { vertexName :: Name }
  deriving (Eq, Ord, Show)

-- | Render a 'ImportGraph' to a 'ByteString' in DOT notation.
renderImportGraph :: ImportGraph -> ByteString
renderImportGraph = export (defaultStyle (friendlyName . vertexName)) . unImportGraph

newtype ImportGraphing m (effects :: [* -> *]) a = ImportGraphing (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadControl term (m effects)                    => MonadControl term (ImportGraphing m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (ImportGraphing m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (ImportGraphing m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (ImportGraphing m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (ImportGraphing m effects)


instance ( Effectful m
         , Member (Reader (SomeOrigin term)) effects
         , Member (Resumable (LoadError term value)) effects
         , Member (State ImportGraph) effects
         , Member Syntax.Identifier syntax
         , MonadAnalysis (Located location term) term value (m effects)
         , term ~ Term (Union syntax) ann
         )
      => MonadAnalysis (Located location term) term value (ImportGraphing m effects) where
  type Effects (Located location term) term value (ImportGraphing m effects) = State ImportGraph ': Effects (Located location term) term value (m effects)

  analyzeTerm eval term@(In _ syntax) = do
    case prj syntax of
      Just (Syntax.Identifier name) -> do
        inclusion (Variable name)
        o <- lookupEnv name
        case o >>= withSomeOrigin originModule . origin . unAddress of
          Just ModuleInfo{..} -> modifyImportGraph (vertex (Variable name) >< vertex (Module moduleName) <>)
          Nothing -> pure ()
        pure ()
      _ -> pure ()
    resumeException
      @(LoadError term value)
      (liftAnalyze analyzeTerm eval term)
      (\yield (LoadError name) -> inclusion (Module name) >> yield [])

  analyzeModule recur m = do
    let name = moduleName (moduleInfo m)
    o <- raise ask
    modifyImportGraph (packageGraph @term o >< vertex (Module name) <>)
    inclusion (Module name)
    liftAnalyze analyzeModule recur m

parentGraph :: SomeOrigin term -> ImportGraph
parentGraph = connect <$> packageGraph <*> moduleGraph

packageGraph :: SomeOrigin term -> ImportGraph
packageGraph = maybe empty (vertex . Package . packageName) . withSomeOrigin originPackage

moduleGraph :: SomeOrigin term -> ImportGraph
moduleGraph = maybe empty (vertex . Module . moduleName) . withSomeOrigin originModule

inclusion :: forall m location term value effects
          .  ( Effectful m
             , Member (Reader (SomeOrigin term)) effects
             , Member (State ImportGraph) effects
             , MonadEvaluator location term value (m effects)
             )
          => Vertex
          -> ImportGraphing m effects ()
inclusion v = do
    o <- raise ask
    modifyImportGraph (parentGraph @term o >< vertex v <>)

(><) :: Graph a => a -> a -> a
(><) = connect

infixr 7 ><

modifyImportGraph :: (Effectful m, Member (State ImportGraph) effects) => (ImportGraph -> ImportGraph) -> ImportGraphing m effects ()
modifyImportGraph = raise . modify'


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
