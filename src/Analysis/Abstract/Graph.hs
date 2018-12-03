{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
import           Control.Effect.Carrier
import           Control.Effect.Sum
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
                 , Member (State (Graph ControlFlowVertex)) sig
                 , Member (State (Map (Slot (Hole context (Located address))) ControlFlowVertex)) sig
                 , AbstractValue term (Hole context (Located address)) value m
                 , Member (State (Heap (Hole context (Located address)) (Hole context (Located address)) value)) sig
                 , Member (State (ScopeGraph (Hole context (Located address)))) sig
                 , Member (Resumable (BaseError (ScopeError (Hole context (Located address))))) sig
                 , Member (Resumable (BaseError (HeapError (Hole context (Located address))))) sig
                 , Member (Reader (Hole context (Located address), Hole context (Located address))) sig
                 , Member (Reader ControlFlowVertex) sig
                 , VertexDeclaration syntax
                 , Declarations1 syntax
                 , Ord address
                 , Show address
                 , Show context
                 , Ord context
                 , Foldable syntax
                 , term ~ Term syntax Location
                 , Carrier sig m
                 )
              => Open (Open (term -> Evaluator term (Hole context (Located address)) value m (ValueRef (Hole context (Located address)) value)))
graphingTerms recur0 recur term@(Term (In a syntax)) = do
  definedInModule <- currentModule
  case toVertex a definedInModule syntax of
    Just (v@Function{}, _) -> recurWithContext v
    Just (v@Method{}, _) -> recurWithContext v
    Just (v@Variable{..}, name) -> do
      variableDefinition v
      addr <- lookupDeclaration (Declaration name)
      defined <- gets (Map.lookup addr)
      maybe (pure ()) (appendGraph . connect (vertex v) . vertex) defined
      recur0 recur term
    _ -> recur0 recur term
  where
    recurWithContext v = do
      variableDefinition v
      moduleInclusion v
      local (const v) $ do
        valRef <- recur0 recur term
        addr <- Control.Abstract.address valRef -- TODO: This is partial, we should remove ValRef
        modify (Map.insert addr v)
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
graphingModules :: ( Member (Modules address value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (State (Graph ControlFlowVertex)) sig
                   , Member (Reader ControlFlowVertex) sig
                   , Carrier sig m
                   )
                => (Module body -> Evaluator term address value (EavesdropC address value (Eff m)) a)
                -> (Module body -> Evaluator term address value m a)
graphingModules recur m = do
  let v = moduleVertex (moduleInfo m)
  appendGraph (vertex v)
  local (const v) $
    eavesdrop (recur m) $ \case
      Load   path _ -> includeModule path
      Lookup path _ -> includeModule path
      _             -> pure ()
  where
    -- NB: path is null for Languages like Ruby that have module imports that require concrete value semantics.
    includeModule path = let path' = if Prologue.null path then "unknown, concrete semantics required" else path
      in moduleInclusion (moduleVertex (ModuleInfo path'))

-- | Add vertices to the graph for imported modules.
graphingModuleInfo :: ( Member (Modules address value) sig
                      , Member (Reader ModuleInfo) sig
                      , Member (State (Graph ModuleInfo)) sig
                      , Carrier sig m
                      )
                   => (Module body -> Evaluator term address value (EavesdropC address value (Eff m)) a)
                   -> (Module body -> Evaluator term address value m a)
graphingModuleInfo recur m = do
  appendGraph (vertex (moduleInfo m))
  eavesdrop (recur m) $ \case
    Load   path _ -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
    Lookup path _ -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path)) . vertex
    _             -> pure ()

eavesdrop :: (Carrier sig m, Member (Modules address value) sig)
          => Evaluator term address value (EavesdropC address value (Eff m)) a
          -> (forall x . Modules address value (Eff m) (Eff m x) -> Evaluator term address value m ())
          -> Evaluator term address value m a
eavesdrop m f = raiseHandler (runEavesdropC (runEvaluator . f) . interpret) m

newtype EavesdropC address value m a = EavesdropC ((forall x . Modules address value m (m x) -> m ()) -> m a)

runEavesdropC :: (forall x . Modules address value m (m x) -> m ()) -> EavesdropC address value m a -> m a
runEavesdropC f (EavesdropC m) = m f

instance (Carrier sig m, Member (Modules address value) sig, Applicative m) => Carrier sig (EavesdropC address value m) where
  ret a = EavesdropC (const (ret a))
  eff op
    | Just eff <- prj op = EavesdropC (\ handler -> let eff' = handlePure (runEavesdropC handler) eff in handler eff' *> send eff')
    | otherwise          = EavesdropC (\ handler -> eff (handlePure (runEavesdropC handler) op))

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
         => Evaluator term address value (StateC (Map (Slot address) ControlFlowVertex) (Eff
                                         (StateC (Graph ControlFlowVertex) (Eff
                                         m)))) result
         -> Evaluator term address value m (Graph ControlFlowVertex, result)
graphing = raiseHandler $ runState mempty . fmap snd . runState lowerBound
