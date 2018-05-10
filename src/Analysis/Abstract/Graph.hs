{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Graph
( Graph(..)
, Vertex(..)
, renderGraph
, appendGraph
, variableDefinition
, moduleInclusion
, packageInclusion
, packageGraph
, graphingTerms
, graphingLoadErrors
, graphingModules
, graphing
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as GC
import           Algebra.Graph.Class hiding (Graph, Vertex)
import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract
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
newtype Graph = Graph { unGraph :: G.Graph Vertex }
  deriving (Eq, GC.Graph, Show)

-- | A vertex of some specific type.
data Vertex
  = Package  { vertexName :: ByteString }
  | Module   { vertexName :: ByteString }
  | Variable { vertexName :: ByteString }
  deriving (Eq, Ord, Show)

-- | Render a 'Graph' to a 'ByteString' in DOT notation.
renderGraph :: Graph -> ByteString
renderGraph = export style . unGraph

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


-- | Add vertices to the graph for evaluated identifiers.
graphingTerms :: ( Element Syntax.Identifier syntax
                 , Members '[ Reader (Environment (Located location) value)
                            , Reader ModuleInfo
                            , Reader PackageInfo
                            , State (Environment (Located location) value)
                            , State Graph
                            ] effects
                 , term ~ Term (Sum syntax) ann
                 )
              => SubtermAlgebra (Base term) term (Evaluator (Located location) value effects a)
              -> SubtermAlgebra (Base term) term (Evaluator (Located location) value effects a)
graphingTerms recur term@(In _ syntax) = do
  case projectSum syntax of
    Just (Syntax.Identifier name) -> do
      moduleInclusion (Variable (unName name))
      variableDefinition name
    _ -> pure ()
  recur term

-- | Add vertices to the graph for 'LoadError's.
graphingLoadErrors :: Members '[ Reader ModuleInfo
                               , Resumable (LoadError location value)
                               , State Graph
                               ] effects
                   => SubtermAlgebra (Base term) term (Evaluator location value effects a)
                   -> SubtermAlgebra (Base term) term (Evaluator location value effects a)
graphingLoadErrors recur term = recur term `resumeLoadError` (\ (ModuleNotFound name) -> moduleInclusion (Module (BC.pack name)) *> moduleNotFound name)

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingModules :: Members '[ Reader ModuleInfo
                            , Reader PackageInfo
                            , State Graph
                            ] effects
               => SubtermAlgebra Module term (Evaluator location value effects a)
               -> SubtermAlgebra Module term (Evaluator location value effects a)
graphingModules recur m = do
  let name = BC.pack (modulePath (moduleInfo m))
  packageInclusion (Module name)
  moduleInclusion (Module name)
  recur m


packageGraph :: PackageInfo -> Graph
packageGraph = vertex . Package . unName . packageName

moduleGraph :: ModuleInfo -> Graph
moduleGraph = vertex . Module . BC.pack . modulePath

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: ( Effectful m
                    , Members '[ Reader PackageInfo
                               , State Graph
                               ] effects
                    , Monad (m effects)
                    )
                 => Vertex
                 -> m effects ()
packageInclusion v = do
  p <- currentPackage
  appendGraph (packageGraph p `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: ( Effectful m
                   , Members '[ Reader ModuleInfo
                              , State Graph
                              ] effects
                   , Monad (m effects)
                   )
                => Vertex
                -> m effects ()
moduleInclusion v = do
  m <- currentModule
  appendGraph (moduleGraph m `connect` vertex v)

-- | Add an edge from the passed variable name to the module it originated within.
variableDefinition :: ( Member (Reader (Environment (Located location) value)) effects
                      , Member (State (Environment (Located location) value)) effects
                      , Member (State Graph) effects
                      )
                   => Name
                   -> Evaluator (Located location) value effects ()
variableDefinition name = do
  graph <- maybe empty (moduleGraph . locationModule . unAddress) <$> lookupEnv name
  appendGraph (vertex (Variable (unName name)) `connect` graph)

appendGraph :: (Effectful m, Member (State Graph) effects) => Graph -> m effects ()
appendGraph = modify' . (<>)


instance Semigroup Graph where
  (<>) = overlay

instance Monoid Graph where
  mempty = empty
  mappend = (<>)

instance Ord Graph where
  compare (Graph G.Empty)           (Graph G.Empty)           = EQ
  compare (Graph G.Empty)           _                               = LT
  compare _                               (Graph G.Empty)           = GT
  compare (Graph (G.Vertex a))      (Graph (G.Vertex b))      = compare a b
  compare (Graph (G.Vertex _))      _                               = LT
  compare _                               (Graph (G.Vertex _))      = GT
  compare (Graph (G.Overlay a1 a2)) (Graph (G.Overlay b1 b2)) = (compare `on` Graph) a1 b1 <> (compare `on` Graph) a2 b2
  compare (Graph (G.Overlay _  _))  _                               = LT
  compare _                               (Graph (G.Overlay _ _))   = GT
  compare (Graph (G.Connect a1 a2)) (Graph (G.Connect b1 b2)) = (compare `on` Graph) a1 b1 <> (compare `on` Graph) a2 b2

instance Output Graph where
  toOutput = toStrict . (<> "\n") . encode

instance ToJSON Graph where
  toJSON Graph{..} = object [ "vertices" .= vertices, "edges" .= edges ]
    where
      vertices = toJSON (G.vertexList unGraph)
      edges = fmap (\(a, b) -> object [ "source" .= vertexToText a, "target" .= vertexToText b ]) (G.edgeList unGraph)

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexToText v, "type" .= vertexToType v ]

vertexToText :: Vertex -> Text
vertexToText = decodeUtf8 . vertexName

vertexToType :: Vertex -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"


graphing :: Effectful m => m (State Graph ': effects) result -> m effects (result, Graph)
graphing = runState mempty
