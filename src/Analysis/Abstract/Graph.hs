{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Graph
( Graph(..)
, Vertex(..)
, style
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

import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract
import           Data.Abstract.Address
import           Data.Abstract.FreeVariables
import           Data.Abstract.Module (Module(moduleInfo), ModuleInfo(..))
import           Data.Abstract.Package (PackageInfo(..))
import           Data.Aeson hiding (Result)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.Graph
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Text.Encoding as T
import           Prologue hiding (packageName)

-- | A vertex of some specific type.
data Vertex
  = Package  { vertexName :: ByteString }
  | Module   { vertexName :: ByteString }
  | Variable { vertexName :: ByteString }
  deriving (Eq, Ord, Show)

style :: Style Vertex Builder
style = (defaultStyle (byteString . vertexName))
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
                            , State (Graph Vertex)
                            ] effects
                 , term ~ Term (Sum syntax) ann
                 )
              => SubtermAlgebra (Base term) term (TermEvaluator term (Located location) value effects a)
              -> SubtermAlgebra (Base term) term (TermEvaluator term (Located location) value effects a)
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
                               , State (Graph Vertex)
                               ] effects
                   => SubtermAlgebra (Base term) term (TermEvaluator term location value effects a)
                   -> SubtermAlgebra (Base term) term (TermEvaluator term location value effects a)
graphingLoadErrors recur term = TermEvaluator (runTermEvaluator (recur term) `resumeLoadError` (\ (ModuleNotFound name) -> moduleInclusion (Module (BC.pack name)) *> moduleNotFound name))

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingModules :: Members '[ Reader ModuleInfo
                            , Reader PackageInfo
                            , State (Graph Vertex)
                            ] effects
               => SubtermAlgebra Module term (TermEvaluator term location value effects a)
               -> SubtermAlgebra Module term (TermEvaluator term location value effects a)
graphingModules recur m = do
  let name = BC.pack (modulePath (moduleInfo m))
  packageInclusion (Module name)
  moduleInclusion (Module name)
  recur m


packageGraph :: PackageInfo -> Graph Vertex
packageGraph = vertex . Package . unName . packageName

moduleGraph :: ModuleInfo -> Graph Vertex
moduleGraph = vertex . Module . BC.pack . modulePath

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: ( Effectful m
                    , Members '[ Reader PackageInfo
                               , State (Graph Vertex)
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
                              , State (Graph Vertex)
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
                      , Member (State (Graph Vertex)) effects
                      )
                   => Name
                   -> TermEvaluator term (Located location) value effects ()
variableDefinition name = do
  graph <- maybe lowerBound (moduleGraph . locationModule . unAddress) <$> TermEvaluator (lookupEnv name)
  appendGraph (vertex (Variable (unName name)) `connect` graph)

appendGraph :: (Effectful m, Member (State (Graph Vertex)) effects) => Graph Vertex -> m effects ()
appendGraph = modify' . (<>)


instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexToText v, "type" .= vertexToType v ]

vertexToText :: Vertex -> Text
vertexToText = decodeUtf8 . vertexName

vertexToType :: Vertex -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"


graphing :: Effectful m => m (State (Graph Vertex) ': effects) result -> m effects (result, Graph Vertex)
graphing = runState mempty
