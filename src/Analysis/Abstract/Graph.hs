{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Graph
( Graph(..)
, Vertex(..)
, renderGraph
, appendGraph
, variableDefinition
, moduleInclusion
, packageInclusion
, packageGraph
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as GC
import           Algebra.Graph.Class hiding (Graph, Vertex)
import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract.Analysis
import           Data.Abstract.Address
import           Data.Abstract.FreeVariables
import           Data.Abstract.Located
import           Data.Abstract.Module hiding (Module)
import           Data.Abstract.Origin hiding (Module, Package)
import           Data.Abstract.Package hiding (Package)
import           Data.Aeson hiding (Result)
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict)
import           Data.Output
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

packageGraph :: SomeOrigin term -> Graph
packageGraph = maybe empty (vertex . Package . unName . packageName) . withSomeOrigin originPackage

moduleGraph :: SomeOrigin term -> Graph
moduleGraph = maybe empty (vertex . Module . BC.pack . modulePath) . withSomeOrigin originModule

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: forall m location term value effects
                 .  ( Member (State Graph) effects
                    , MonadEvaluator location term value effects m
                    )
                 => Vertex
                 -> m effects ()
packageInclusion v = do
  o <- raise ask
  appendGraph (packageGraph @term o `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: forall m location term value effects
                .  ( Member (State Graph) effects
                   , MonadEvaluator location term value effects m
                   )
                => Vertex
                -> m effects ()
moduleInclusion v = do
  o <- raise ask
  appendGraph (moduleGraph @term o `connect` vertex v)

-- | Add an edge from the passed variable name to the module it originated within.
variableDefinition :: ( Member (State Graph) effects
                      , MonadEvaluator (Located location term) term value effects m
                      )
                   => Name
                   -> m effects ()
variableDefinition name = do
  graph <- maybe empty (moduleGraph . origin . unAddress) <$> lookupEnv name
  appendGraph (vertex (Variable (unName name)) `connect` graph)

appendGraph :: (Effectful m, Member (State Graph) effects) => Graph -> m effects ()
appendGraph = raise . modify' . (<>)


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
