{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Graph
( Graph(..)
, Vertex(..)
, moduleVertex
, style
, appendGraph
, variableDefinition
, moduleInclusion
, packageInclusion
, graphingTerms
, graphingPackages
, graphingModules
, graphingModuleInfo
, graphing
) where

import           Algebra.Graph.Export.Dot hiding (vertexName)
import           Control.Abstract
import           Data.Abstract.Address
import           Data.Abstract.Module (Module(moduleInfo), ModuleInfo(..))
import           Data.Abstract.Name
import           Data.Abstract.Package (PackageInfo(..))
import           Data.ByteString.Builder
import           Data.Graph
import           Data.Graph.Vertex
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import qualified Data.Text.Encoding as T
import           Prologue hiding (project)

style :: Style Vertex Builder
style = (defaultStyle (T.encodeUtf8Builder . vertexName))
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
                 , Member (Reader ModuleInfo) effects
                 , Member (Env (Hole (Located address))) effects
                 , Member (State (Graph Vertex)) effects
                 , Base term ~ TermF (Sum syntax) ann
                 )
              => SubtermAlgebra (Base term) term (TermEvaluator term (Hole (Located address)) value effects a)
              -> SubtermAlgebra (Base term) term (TermEvaluator term (Hole (Located address)) value effects a)
graphingTerms recur term@(In _ syntax) = do
  case project syntax of
    Just (Syntax.Identifier name) -> do
      moduleInclusion (Variable (formatName name))
      variableDefinition name
    _ -> pure ()
  recur term

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingPackages :: ( Member (Reader PackageInfo) effects
                    , Member (State (Graph Vertex)) effects
                    )
                 => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                 -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingPackages recur m = packageInclusion (moduleVertex (moduleInfo m)) *> recur m

-- | Add vertices to the graph for imported modules.
graphingModules :: forall term address value effects a
                .  ( Effects effects
                   , Member (Modules address) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (State (Graph Vertex)) effects
                   )
                => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingModules recur m = do
  appendGraph (vertex (moduleVertex (moduleInfo m)))
  eavesdrop @(Modules address) (\ m -> case m of
    Load   path -> moduleInclusion (moduleVertex (ModuleInfo path))
    Lookup path -> moduleInclusion (moduleVertex (ModuleInfo path))
    _ -> pure ())
    (recur m)

-- | Add vertices to the graph for imported modules.
graphingModuleInfo :: forall term address value effects a
                   .  ( Effects effects
                      , Member (Modules address) effects
                      , Member (Reader ModuleInfo) effects
                      , Member (State (Graph ModuleInfo)) effects
                      )
                   => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                   -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingModuleInfo recur m = do
  appendGraph (vertex (moduleInfo m))
  eavesdrop @(Modules address) (\ eff -> case eff of
    Load   path -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
    Lookup path -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
    _ -> pure ())
    (recur m)

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
variableDefinition :: ( Member (Env (Hole (Located address))) effects
                      , Member (State (Graph Vertex)) effects
                      )
                   => Name
                   -> TermEvaluator term (Hole (Located address)) value effects ()
variableDefinition name = do
  graph <- maybe lowerBound (maybe lowerBound (vertex . moduleVertex . addressModule) . toMaybe) <$> TermEvaluator (lookupEnv name)
  appendGraph (vertex (Variable (formatName name)) `connect` graph)

appendGraph :: (Effectful m, Member (State (Graph v)) effects) => Graph v -> m effects ()
appendGraph = modify' . (<>)


graphing :: (Effectful m, Effects effects) => m (State (Graph Vertex) ': effects) result -> m effects (Graph Vertex, result)
graphing = runState mempty
