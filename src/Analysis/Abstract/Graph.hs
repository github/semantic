{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Graph
( Graph(..)
, Vertex(..)
, style
, appendGraph
, variableDefinition
, moduleInclusion
, packageInclusion
, graphingTerms
, graphingPackages
, graphingModules
, graphing
) where

import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract
import           Data.Abstract.Address
import           Data.Abstract.Module (Module(moduleInfo), ModuleInfo(..))
import           Data.Abstract.Name
import           Data.Abstract.Package (PackageInfo(..))
import           Data.Aeson hiding (Result)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.Graph
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Text.Encoding as T
import           Prologue hiding (packageName, project)

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
                 , Member (Reader (Environment (Hole (Located address)))) effects
                 , Member (Reader ModuleInfo) effects
                 , Member (Env (Hole (Located address))) effects
                 , Member (State (Graph Vertex)) effects
                 , term ~ Term (Sum syntax) ann
                 )
              => SubtermAlgebra (Base term) term (TermEvaluator term (Hole (Located address)) value effects a)
              -> SubtermAlgebra (Base term) term (TermEvaluator term (Hole (Located address)) value effects a)
graphingTerms recur term@(In _ syntax) = do
  case project syntax of
    Just (Syntax.Identifier name) -> do
      moduleInclusion (Variable (unName name))
      variableDefinition name
    _ -> pure ()
  recur term

graphingPackages :: ( Member (Reader PackageInfo) effects
                    , Member (State (Graph Vertex)) effects
                    )
                 => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                 -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingPackages recur m = packageInclusion (moduleVertex (moduleInfo m)) *> recur m

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingModules :: forall term address value effects a
                .  ( Member (Modules address value) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (State (Graph Vertex)) effects
                   )
               => SubtermAlgebra Module term (TermEvaluator term address value effects a)
               -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingModules recur m = interpose @(Modules address value) pure (\ m yield -> case m of
  Load   path -> moduleInclusion (moduleVertex (ModuleInfo path)) >> send m >>= yield
  Lookup path -> moduleInclusion (moduleVertex (ModuleInfo path)) >> send m >>= yield
  _ -> send m >>= yield)
  (recur m)


packageVertex :: PackageInfo -> Vertex
packageVertex = Package . unName . packageName

moduleVertex :: ModuleInfo -> Vertex
moduleVertex = Module . BC.pack . modulePath

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: ( Effectful m
                    , Member (Reader PackageInfo) effects
                    , Member (State (Graph Vertex)) effects
                    , Monad (m effects)
                    )
                 => Vertex
                 -> m effects ()
packageInclusion v = do
  p <- currentPackage
  appendGraph (vertex (packageVertex p) `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: ( Effectful m
                   , Member (Reader ModuleInfo) effects
                   , Member (State (Graph Vertex)) effects
                   , Monad (m effects)
                   )
                => Vertex
                -> m effects ()
moduleInclusion v = do
  m <- currentModule
  appendGraph (vertex (moduleVertex m) `connect` vertex v)

-- | Add an edge from the passed variable name to the module it originated within.
variableDefinition :: ( Member (Reader (Environment (Hole (Located address)))) effects
                      , Member (Env (Hole (Located address))) effects
                      , Member (State (Graph Vertex)) effects
                      )
                   => Name
                   -> TermEvaluator term (Hole (Located address)) value effects ()
variableDefinition name = do
  graph <- maybe lowerBound (maybe lowerBound (vertex . moduleVertex . addressModule) . toMaybe) <$> TermEvaluator (lookupEnv name)
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
