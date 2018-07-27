{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Graph
( Graph(..)
, Vertex(..)
, moduleVertex
, unknownModuleVertex
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
import           Control.Abstract hiding (Function(..))
import           Data.Abstract.Address
import           Data.Abstract.Ref
import           Data.Abstract.Declarations
import           Data.Abstract.Module (Module (moduleInfo), ModuleInfo (..))
import           Data.Abstract.Package (PackageInfo (..))
import           Data.ByteString.Builder
import           Data.Graph
import           Data.Graph.Vertex
import           Data.Record
import           Data.Term
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import           Prologue hiding (project)

style :: Style Vertex Builder
style = (defaultStyle (T.encodeUtf8Builder . vertexIdentifier))
  { vertexAttributes = vertexAttributes
  , edgeAttributes   = edgeAttributes
  }
  where vertexAttributes Package{}       = [ "style" := "dashed", "shape" := "box" ]
        vertexAttributes Module{}        = [ "style" := "dotted, rounded", "shape" := "box" ]
        vertexAttributes UnknownModule{} = [ "style" := "dotted, rounded", "shape" := "box", "color" := "red", "fontcolor" := "red" ]
        vertexAttributes Variable{..} = [ "label" := T.encodeUtf8Builder (vertexName <> " (Variable)"), "tooltip" := T.encodeUtf8Builder (showSpan vertexSpan), "style" := "rounded", "shape" := "box" ]
        vertexAttributes Method{..}   = [ "label" := T.encodeUtf8Builder (vertexName <> " (Method)"),   "tooltip" := T.encodeUtf8Builder (showSpan vertexSpan)  , "style" := "rounded", "shape" := "box" ]
        vertexAttributes Function{..} = [ "label" := T.encodeUtf8Builder (vertexName <> " (Function)"), "tooltip" := T.encodeUtf8Builder (showSpan vertexSpan), "style" := "rounded", "shape" := "box" ]
        edgeAttributes Module{}   Module{}          = [ "len" := "5.0", "label" := "imports" ]
        edgeAttributes Module{}   UnknownModule{}   = [ "len" := "5.0", "label" := "imports" ]
        edgeAttributes Package{}  Module{}   = [ "len" := "5.0", "style" := "dashed" ]
        edgeAttributes Variable{} Module{}   = [ "len" := "5.0", "color" := "blue", "label" := "refers to symbol defined in" ]
        edgeAttributes _          Module{}   = [ "len" := "5.0", "color" := "blue", "label" := "defined in" ]
        edgeAttributes Method{}   Variable{} = [ "len" := "2.0", "color" := "green", "label" := "calls" ]
        edgeAttributes Function{} Variable{} = [ "len" := "2.0", "color" := "green", "label" := "calls" ]
        edgeAttributes Module{}   Function{} = [ "len" := "2.0", "color" := "red", "label" := "defines" ]
        edgeAttributes Module{}   Method{}   = [ "len" := "2.0", "color" := "red", "label" := "defines" ]
        edgeAttributes Module{}   _          = [ "len" := "2.0", "color" := "green", "label" := "calls" ]
        edgeAttributes Variable{} Function{} = [ "len" := "2.0", "color" := "blue", "label" := "references" ]
        edgeAttributes Variable{} Method{}   = [ "len" := "2.0", "color" := "blue", "label" := "references" ]
        edgeAttributes _          _          = []


-- | Add vertices to the graph for evaluated identifiers.
graphingTerms :: ( Member (Reader ModuleInfo) effects
                 , Member (Env (Hole context (Located address))) effects
                 , Member (State (Graph Vertex)) effects
                 , Member (State (Map (Hole context (Located address)) Vertex)) effects
                 , Member (Resumable (EnvironmentError (Hole context (Located address)))) effects
                 , AbstractValue (Hole context (Located address)) value effects
                 , Member (Reader Vertex) effects
                 , HasField fields Span
                 , VertexDeclaration syntax
                 , Declarations1 syntax
                 , Ord address
                 , Ord context
                 , Foldable syntax
                 , Functor syntax
                 , term ~ Term syntax (Record fields)
                 )
              => SubtermAlgebra (Base term) term (TermEvaluator term (Hole context (Located address)) value effects (ValueRef (Hole context (Located address))))
              -> SubtermAlgebra (Base term) term (TermEvaluator term (Hole context (Located address)) value effects (ValueRef (Hole context (Located address))))
graphingTerms recur term@(In a syntax) = do
  definedInModule <- currentModule
  case toVertex a definedInModule (subterm <$> syntax) of
    Just (v@Function{}, _) -> recurWithContext v
    Just (v@Method{}, _) -> recurWithContext v
    Just (v@Variable{..}, name) -> do
      variableDefinition v
      maybeAddr <- TermEvaluator (lookupEnv name)
      case maybeAddr of
        Just a -> do
          defined <- gets (Map.lookup a)
          maybe (pure ()) (appendGraph . connect (vertex v) . vertex) defined
        _ -> pure ()
      recur term
    _ -> recur term
  where
    recurWithContext v = do
      variableDefinition v
      moduleInclusion v
      local (const v) $ do
        valRef <- recur term
        addr <- TermEvaluator (Control.Abstract.address valRef)
        modify' (Map.insert addr v)
        pure valRef

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingPackages :: ( Member (Reader PackageInfo) effects
                    , Member (State (Graph Vertex)) effects
                    , Member (Reader Vertex) effects
                    )
                 => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                 -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingPackages recur m =
  let v = moduleVertex (moduleInfo m) in packageInclusion v *> local (const v) (recur m)

-- | Add vertices to the graph for imported modules.
graphingModules :: forall term address value effects a
                .  ( Member (Modules address) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (State (Graph Vertex)) effects
                   , Member (Reader Vertex) effects
                   , PureEffects effects
                   )
                => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingModules recur m = do
  let v = moduleVertex (moduleInfo m)
  appendGraph (vertex v)
  local (const v) $
    eavesdrop @(Modules address) (\ m -> case m of
      Load path -> includeModule path
      Lookup path -> includeModule path
      _ -> pure ())
      (recur m)
  where
    -- NB: path is null for Languages like Ruby that have module imports that require concrete value semantics.
    includeModule path = let path' = if Prologue.null path then "unknown, concrete semantics required" else path
      in moduleInclusion (moduleVertex (ModuleInfo path'))

-- | Add vertices to the graph for imported modules.
graphingModuleInfo :: forall term address value effects a
                   .  ( Member (Modules address) effects
                      , Member (Reader ModuleInfo) effects
                      , Member (State (Graph ModuleInfo)) effects
                      , PureEffects effects
                      )
                   => SubtermAlgebra Module term (TermEvaluator term address value effects a)
                   -> SubtermAlgebra Module term (TermEvaluator term address value effects a)
graphingModuleInfo recur m = do
  appendGraph (vertex (moduleInfo m))
  eavesdrop @(Modules address) (\ eff -> case eff of
    Load path -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
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

-- | Add an edge from the passed variable name to the context it originated within.
variableDefinition :: ( Member (State (Graph Vertex)) effects
                      , Member (Reader Vertex) effects
                      )
                   => Vertex
                   -> TermEvaluator term (Hole context (Located address)) value effects ()
variableDefinition var = do
  context <- ask
  appendGraph $ vertex context `connect` vertex var

appendGraph :: (Effectful m, Member (State (Graph v)) effects) => Graph v -> m effects ()
appendGraph = modify' . (<>)


graphing :: (Effectful m, Effects effects, Functor (m (State (Graph Vertex) : effects)))
         => m (State (Map (Hole context (Located address)) Vertex) ': State (Graph Vertex) ': effects) result -> m effects (Graph Vertex, result)
graphing = runState mempty . fmap snd . runState lowerBound
