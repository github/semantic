{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.ImportGraph
( ImportGraph(..)
, renderImportGraph
, graphingTerms
, graphingModules
, importGraphing
) where

import qualified Algebra.Graph as G
import           Algebra.Graph.Class hiding (Vertex)
import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract.Evaluator
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable (LoadError (..))
import           Data.Abstract.FreeVariables
import           Data.Abstract.Located
import           Data.Abstract.Module (Module(moduleInfo), ModuleInfo(..))
import           Data.Abstract.Package (PackageInfo(..))
import           Data.Aeson hiding (Result)
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict)
import           Data.Output
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Text.Encoding as T
import           Prologue hiding (empty, packageName)

-- | The graph of function variableDefinitions to symbols used in a given program.
newtype ImportGraph term = ImportGraph { unImportGraph :: G.Graph (Vertex term) }
  deriving (Eq, Graph, Show)

-- | A vertex of some specific type.
data Vertex term
  = Package  { vertexName :: ByteString }
  | Module   { vertexName :: ByteString }
  | Variable { vertexName :: ByteString }
  deriving (Eq, Ord, Show)

-- | Render a 'ImportGraph' to a 'ByteString' in DOT notation.
renderImportGraph :: ImportGraph term -> ByteString
renderImportGraph = export style . unImportGraph

style :: Style (Vertex term) ByteString
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

graphingTerms :: forall location term value effects syntax ann a
              .  ( Element Syntax.Identifier syntax
                 , Members '[ Reader (Environment (Located location) value)
                            , Reader ModuleInfo
                            , Reader PackageInfo
                            , Resumable (LoadError term)
                            , State (Environment (Located location) value)
                            , State (ImportGraph term)
                            ] effects
                 , term ~ Term (Sum syntax) ann
                 )
              => SubtermAlgebra (Base term) term (Evaluator (Located location) term value effects a)
              -> SubtermAlgebra (Base term) term (Evaluator (Located location) term value effects a)
graphingTerms recur term@(In _ syntax) = do
  case projectSum syntax of
    Just (Syntax.Identifier name) -> do
      moduleInclusion (Variable (unName name))
      variableDefinition name
    _ -> pure ()
  resume
    @(LoadError term)
    (recur term)
    (\yield (LoadError name) -> moduleInclusion (Module (BC.pack name)) *> yield [])

graphingModules :: Members '[ Reader ModuleInfo
                            , Reader PackageInfo
                            , State (ImportGraph term)
                            ] effects
               => SubtermAlgebra Module term (Evaluator location term value effects a)
               -> SubtermAlgebra Module term (Evaluator location term value effects a)
graphingModules recur m = do
  let name = BC.pack (modulePath (moduleInfo m))
  packageInclusion (Module name)
  moduleInclusion (Module name)
  recur m


packageGraph :: PackageInfo -> ImportGraph term
packageGraph = vertex . Package . unName . packageName

moduleGraph :: ModuleInfo -> ImportGraph term
moduleGraph = vertex . Module . BC.pack . modulePath

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: Members '[ Reader PackageInfo
                             , State (ImportGraph term)
                             ] effects
                 => Vertex term
                 -> Evaluator location term value effects ()
packageInclusion v = do
  p <- currentPackage
  appendGraph (packageGraph p `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: Members '[ Reader ModuleInfo
                            , State (ImportGraph term)
                            ] effects
                => Vertex term
                -> Evaluator location term value effects ()
moduleInclusion v = do
  m <- currentModule
  appendGraph (moduleGraph m `connect` vertex v)

-- | Add an edge from the passed variable name to the module it originated within.
variableDefinition :: ( Member (Reader (Environment (Located location) value)) effects
                      , Member (State (Environment (Located location) value)) effects
                      , Member (State (ImportGraph term)) effects
                      )
                   => Name
                   -> Evaluator (Located location) term value effects ()
variableDefinition name = do
  graph <- maybe empty (moduleGraph . locationModule . unAddress) <$> lookupEnv name
  appendGraph (vertex (Variable (unName name)) `connect` graph)

appendGraph :: Member (State (ImportGraph term)) effects => ImportGraph term -> Evaluator location term value effects ()
appendGraph = raise . modify' . (<>)


instance Semigroup (ImportGraph term) where
  (<>) = overlay

instance Monoid (ImportGraph term) where
  mempty = empty
  mappend = (<>)

instance Ord (ImportGraph term) where
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

instance Output (ImportGraph term) where
  toOutput = toStrict . (<> "\n") . encode

instance ToJSON (ImportGraph term) where
  toJSON ImportGraph{..} = object [ "vertices" .= vertices, "edges" .= edges ]
    where
      vertices = toJSON (G.vertexList unImportGraph)
      edges = fmap (\(a, b) -> object [ "source" .= vertexToText a, "target" .= vertexToText b ]) (G.edgeList unImportGraph)

instance ToJSON (Vertex term) where
  toJSON v = object [ "name" .= vertexToText v, "type" .= vertexToType v ]

vertexToText :: Vertex termt -> Text
vertexToText = decodeUtf8 . vertexName

vertexToType :: Vertex termt -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"


importGraphing :: Effectful m => m (State (ImportGraph term) ': effects) result -> m effects (result, ImportGraph term)
importGraphing = runState mempty
