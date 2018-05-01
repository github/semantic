{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.CallGraph
( CallGraph(..)
, renderCallGraph
, CallGraphing
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
import           Data.Aeson hiding (Result)
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict)
import           Data.Output
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Text.Encoding as T
import           Prologue hiding (empty, packageName)

-- | The graph of function variableDefinitions to symbols used in a given program.
newtype CallGraph = CallGraph { unCallGraph :: G.Graph Vertex }
  deriving (Eq, Graph, Show)

-- | A vertex of some specific type.
data Vertex
  = Package  { vertexName :: ByteString }
  | Module   { vertexName :: ByteString }
  | Variable { vertexName :: ByteString }
  deriving (Eq, Ord, Show)

-- | Render a 'CallGraph' to a 'ByteString' in DOT notation.
renderCallGraph :: CallGraph -> ByteString
renderCallGraph = export style . unCallGraph

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

newtype CallGraphing m (effects :: [* -> *]) a = CallGraphing { runCallGraphing :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (CallGraphing m)


instance ( Effectful m
         , Member (Resumable (LoadError term)) effects
         , Member (State CallGraph) effects
         , Member Syntax.Identifier syntax
         , MonadAnalysis (Located location term) term value effects m
         , term ~ Term (Union syntax) ann
         )
      => MonadAnalysis (Located location term) term value effects (CallGraphing m) where
  analyzeTerm eval term@(In _ syntax) = do
    case prj syntax of
      Just (Syntax.Identifier name) -> do
        moduleInclusion (Variable (unName name))
        variableDefinition name
      _ -> pure ()
    resume
      @(LoadError term)
      (liftAnalyze analyzeTerm eval term)
      (\yield (LoadError name) -> moduleInclusion (Module (BC.pack name)) >> yield [])

  analyzeModule recur m = do
    let name = BC.pack (modulePath (moduleInfo m))
    packageInclusion (Module name)
    moduleInclusion (Module name)
    liftAnalyze analyzeModule recur m

packageGraph :: SomeOrigin term -> CallGraph
packageGraph = maybe empty (vertex . Package . unName . packageName) . withSomeOrigin originPackage

moduleGraph :: SomeOrigin term -> CallGraph
moduleGraph = maybe empty (vertex . Module . BC.pack . modulePath) . withSomeOrigin originModule

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: forall m location term value effects
                 .  ( Member (State CallGraph) effects
                    , MonadEvaluator location term value effects m
                    )
                 => Vertex
                 -> CallGraphing m effects ()
packageInclusion v = do
  o <- raise ask
  appendGraph (packageGraph @term o `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: forall m location term value effects
                .  ( Member (State CallGraph) effects
                   , MonadEvaluator location term value effects m
                   )
                => Vertex
                -> CallGraphing m effects ()
moduleInclusion v = do
  o <- raise ask
  appendGraph (moduleGraph @term o `connect` vertex v)

-- | Add an edge from the passed variable name to the module it originated within.
variableDefinition :: ( Member (State CallGraph) effects
                      , MonadEvaluator (Located location term) term value effects m
                      )
                   => Name
                   -> CallGraphing m effects ()
variableDefinition name = do
  graph <- maybe empty (moduleGraph . origin . unAddress) <$> lookupEnv name
  appendGraph (vertex (Variable (unName name)) `connect` graph)

appendGraph :: (Effectful m, Member (State CallGraph) effects) => CallGraph -> CallGraphing m effects ()
appendGraph = raise . modify' . (<>)


instance Semigroup CallGraph where
  (<>) = overlay

instance Monoid CallGraph where
  mempty = empty
  mappend = (<>)

instance Ord CallGraph where
  compare (CallGraph G.Empty)           (CallGraph G.Empty)           = EQ
  compare (CallGraph G.Empty)           _                               = LT
  compare _                               (CallGraph G.Empty)           = GT
  compare (CallGraph (G.Vertex a))      (CallGraph (G.Vertex b))      = compare a b
  compare (CallGraph (G.Vertex _))      _                               = LT
  compare _                               (CallGraph (G.Vertex _))      = GT
  compare (CallGraph (G.Overlay a1 a2)) (CallGraph (G.Overlay b1 b2)) = (compare `on` CallGraph) a1 b1 <> (compare `on` CallGraph) a2 b2
  compare (CallGraph (G.Overlay _  _))  _                               = LT
  compare _                               (CallGraph (G.Overlay _ _))   = GT
  compare (CallGraph (G.Connect a1 a2)) (CallGraph (G.Connect b1 b2)) = (compare `on` CallGraph) a1 b1 <> (compare `on` CallGraph) a2 b2

instance Output CallGraph where
  toOutput = toStrict . (<> "\n") . encode

instance ToJSON CallGraph where
  toJSON CallGraph{..} = object [ "vertices" .= vertices, "edges" .= edges ]
    where
      vertices = toJSON (G.vertexList unCallGraph)
      edges = fmap (\(a, b) -> object [ "source" .= vertexToText a, "target" .= vertexToText b ]) (G.edgeList unCallGraph)

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexToText v, "type" .= vertexToType v ]

vertexToText :: Vertex -> Text
vertexToText = decodeUtf8 . vertexName

vertexToType :: Vertex -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"


instance Interpreter m effects
      => Interpreter (CallGraphing m) (State CallGraph ': effects) where
  type Result (CallGraphing m) (State CallGraph ': effects) result = Result m effects (result, CallGraph)
  interpret = interpret . runCallGraphing . raiseHandler (`runState` mempty)
