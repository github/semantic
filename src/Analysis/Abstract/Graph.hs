{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Graph
( Graph(..)
, ControlFlowVertex(..)
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
import           Data.Abstract.Address.Hole
import           Data.Abstract.Address.Located
import           Data.Abstract.BaseError
import           Data.Abstract.Environment
import           Data.Abstract.Ref
import           Data.Abstract.Declarations
import           Data.Abstract.Module (Module (moduleInfo), ModuleInfo (..))
import           Data.Abstract.Package (PackageInfo (..))
import           Data.ByteString.Builder
import           Data.Graph
import           Data.Graph.ControlFlowVertex
import           Data.Term
import           Data.Location
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import           Prologue

style :: Style ControlFlowVertex Builder
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
graphingTerms :: ( Member (Reader ModuleInfo) sig
                 , Member (Reader Span) sig
                 , Member (Env (Hole context (Located address))) sig
                 , Member (State (Graph ControlFlowVertex)) sig
                 , Member (State (Map (Hole context (Located address)) ControlFlowVertex)) sig
                 , Member (Resumable (BaseError (EnvironmentError (Hole context (Located address))))) sig
                 , AbstractValue term (Hole context (Located address)) value m
                 , Member (Reader ControlFlowVertex) sig
                 , VertexDeclaration syntax
                 , Declarations1 syntax
                 , Ord address
                 , Ord context
                 , Foldable syntax
                 , term ~ Term syntax Location
                 , Carrier sig m
                 )
              => Open (Open (term -> Evaluator term (Hole context (Located address)) value m (ValueRef (Hole context (Located address)))))
graphingTerms recur0 recur term@(Term (In a syntax)) = do
  definedInModule <- currentModule
  case toVertex a definedInModule syntax of
    Just (v@Function{}, _) -> recurWithContext v
    Just (v@Method{}, _) -> recurWithContext v
    Just (v@Variable{..}, name) -> do
      variableDefinition v
      maybeAddr <- lookupEnv name
      case maybeAddr of
        Just a -> do
          defined <- gets (Map.lookup a)
          maybe (pure ()) (appendGraph . connect (vertex v) . vertex) defined
        _ -> pure ()
      recur0 recur term
    _ -> recur0 recur term
  where
    recurWithContext v = do
      variableDefinition v
      moduleInclusion v
      local (const v) $ do
        valRef <- recur0 recur term
        addr <- Control.Abstract.address valRef
        modify' (Map.insert addr v)
        pure valRef

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingPackages :: ( Member (Reader PackageInfo) sig
                    , Member (State (Graph ControlFlowVertex)) sig
                    , Member (Reader ControlFlowVertex) sig
                    , Carrier sig m
                    , Monad m
                    )
                 => Open (Module term -> m a)
graphingPackages recur m =
  let v = moduleVertex (moduleInfo m) in packageInclusion v *> local (const v) (recur m)

-- | Add vertices to the graph for imported modules.
graphingModules :: forall term address value sig m a
                .  ( Member (Modules address) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (State (Graph ControlFlowVertex)) sig
                   , Member (Reader ControlFlowVertex) sig
                   , Carrier sig m
                   )
                => Open (Module term -> Evaluator term address value m a)
graphingModules recur m = do
  let v = moduleVertex (moduleInfo m)
  appendGraph (vertex v)
  local (const v) $
    eavesdrop @(Modules address) (\ m -> case m of
      Load   path -> includeModule path
      Lookup path -> includeModule path
      _           -> pure ())
      (recur m)
  where
    -- NB: path is null for Languages like Ruby that have module imports that require concrete value semantics.
    includeModule path = let path' = if Prologue.null path then "unknown, concrete semantics required" else path
      in moduleInclusion (moduleVertex (ModuleInfo path'))

-- | Add vertices to the graph for imported modules.
graphingModuleInfo :: forall term address value sig m a
                   .  ( Member (Modules address) sig
                      , Member (Reader ModuleInfo) sig
                      , Member (State (Graph ModuleInfo)) sig
                      , Carrier sig m
                      )
                   => Open (Module term -> Evaluator term address value m a)
graphingModuleInfo recur m = do
  appendGraph (vertex (moduleInfo m))
  eavesdrop @(Modules address) (\ eff -> case eff of
    Load   path -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
    Lookup path -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
    _           -> pure ())
    (recur m)

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: ( Member (Reader PackageInfo) sig
                    , Member (State (Graph ControlFlowVertex)) sig
                    , Carrier sig m
                    , Monad m
                    )
                 => ControlFlowVertex
                 -> m ()
packageInclusion v = do
  p <- currentPackage
  appendGraph (vertex (packageVertex p) `connect` vertex v)

-- | Add an edge from the current module to the passed vertex.
moduleInclusion :: ( Member (Reader ModuleInfo) sig
                   , Member (State (Graph ControlFlowVertex)) sig
                   , Carrier sig m
                   , Monad m
                   )
                => ControlFlowVertex
                -> m ()
moduleInclusion v = do
  m <- currentModule
  appendGraph (vertex (moduleVertex m) `connect` vertex v)

-- | Add an edge from the passed variable name to the context it originated within.
variableDefinition :: ( Member (State (Graph ControlFlowVertex)) sig
                      , Member (Reader ControlFlowVertex) sig
                      , Carrier sig m
                      , Monad m
                      )
                   => ControlFlowVertex
                   -> m ()
variableDefinition var = do
  context <- ask
  appendGraph (vertex context `connect` vertex var)

appendGraph :: (Member (State (Graph v)) sig, Carrier sig m, Monad m) => Graph v -> m ()
appendGraph = modify . (<>)


graphing :: (Carrier sig m, Effect sig)
         => Evaluator term address value (StateC (Map address ControlFlowVertex)
           (Evaluator term address value (StateC (Graph ControlFlowVertex)
           (Evaluator term address value m)))) result
         -> Evaluator term address value m (Graph ControlFlowVertex, result)
graphing = runState mempty . runEvaluator . fmap snd . runState lowerBound . runEvaluator
