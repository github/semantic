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
import qualified Data.ByteString.Char8 as BC
import qualified Data.Syntax as Syntax
import           Data.Term
import           Prologue hiding (empty, packageName)

import Data.Aeson
import Data.Output
import Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)

-- | The graph of function variableDefinitions to symbols used in a given program.
newtype ImportGraph = ImportGraph { unImportGraph :: G.Graph Vertex }
  deriving (Eq, Graph, Show)

instance Output ImportGraph where
  toOutput = toStrict . (<> "\n") . encode

instance ToJSON ImportGraph where
  toJSON ImportGraph{..} = object [ "vertices" .= vertices, "edges" .= edges ]
    where
      vertices = toJSON (G.vertexList unImportGraph)
      edges = fmap (\(a, b) -> object [ "source" .= vertexToText a, "target" .= vertexToText b ]) (G.edgeList unImportGraph)

vertexToText :: Vertex -> Text
vertexToText = decodeUtf8 . vertexName

vertexToType :: Vertex -> Text
vertexToType Package{..} = "package"
vertexToType Module{..} = "module"
vertexToType Variable{..} = "variable"

-- | A vertex of some specific type.
data Vertex
  = Package  { vertexName :: ByteString }
  | Module   { vertexName :: ByteString }
  | Variable { vertexName :: ByteString }
  deriving (Eq, Ord, Show)

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexToText v, "type" .= vertexToType v ]

-- | Render a 'ImportGraph' to a 'ByteString' in DOT notation.
renderImportGraph :: ImportGraph -> ByteString
renderImportGraph = export style . unImportGraph

style :: Style Vertex ByteString
style = (defaultStyle vertexName)
  { vertexAttributes = vertexAttributes
  , edgeAttributes   = edgeAttributes
  }
  where vertexAttributes Package{}  = [ "style" := "dashed", "shape" := "box" ]
        vertexAttributes Module{}   = [ "style" := "dotted, rounded", "shape" := "box" ]
        vertexAttributes Variable{} = []
        edgeAttributes Package{}  Module{}   = [ "style" := "dashed" ]
        edgeAttributes Module{}   Variable{} = [ "style" := "dotted" ]
        edgeAttributes Variable{} Module{}   = [ "color" := "blue" ]
        edgeAttributes _          _          = []

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
        moduleInclusion (Variable (unName name))
        variableDefinition name
      _ -> pure ()
    resumeException
      @(LoadError term value)
      (liftAnalyze analyzeTerm eval term)
      (\yield (LoadError name) -> moduleInclusion (Module (BC.pack name)) >> yield [])

  analyzeModule recur m = do
    let name = BC.pack (modulePath (moduleInfo m))
    packageInclusion (Module name)
    moduleInclusion (Module name)
    liftAnalyze analyzeModule recur m

packageGraph :: SomeOrigin term -> ImportGraph
packageGraph = maybe empty (vertex . Package . unName . packageName) . withSomeOrigin originPackage

moduleGraph :: SomeOrigin term -> ImportGraph
moduleGraph = maybe empty (vertex . Module . BC.pack . modulePath) . withSomeOrigin originModule

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: forall m location term value effects
                 .  ( Effectful m
                    , Member (Reader (SomeOrigin term)) effects
                    , Member (State ImportGraph) effects
                    , MonadEvaluator location term value (m effects)
                    )
                 => Vertex
                 -> ImportGraphing m effects ()
packageInclusion v = do
  o <- raise ask
  appendGraph (packageGraph @term o `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: forall m location term value effects
                .  ( Effectful m
                   , Member (Reader (SomeOrigin term)) effects
                   , Member (State ImportGraph) effects
                   , MonadEvaluator location term value (m effects)
                   )
                => Vertex
                -> ImportGraphing m effects ()
moduleInclusion v = do
  o <- raise ask
  appendGraph (moduleGraph @term o `connect` vertex v)

-- | Add an edge from the passed variable name to the module it originated within.
variableDefinition :: ( Effectful m
              , Member (State ImportGraph) effects
              , MonadEvaluator (Located location term) term value (m effects)
              ) => Name -> ImportGraphing m effects ()
variableDefinition name = do
  graph <- maybe empty (moduleGraph . origin . unAddress) <$> lookupEnv name
  appendGraph (vertex (Variable (unName name)) `connect` graph)

appendGraph :: (Effectful m, Member (State ImportGraph) effects) => ImportGraph -> ImportGraphing m effects ()
appendGraph = raise . modify' . (<>)


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
