{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, OverloadedStrings, RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
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
import           Data.Abstract.BaseError
import           Data.Abstract.Module (Module (moduleInfo), ModuleInfo (..))
import           Data.Abstract.Package (PackageInfo (..))
import           Data.ByteString.Builder
import           Data.Graph
import           Data.Graph.ControlFlowVertex
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import           Prologue
import           Source.Loc

style :: Style ControlFlowVertex Builder
style = (defaultStyle (T.encodeUtf8Builder . vertexIdentifier))
  { vertexAttributes = vertexAttributes
  , edgeAttributes   = edgeAttributes
  }
  where vertexAttributes Package{}        = [ "style" := "dashed", "shape" := "box" ]
        vertexAttributes Module{}         = [ "style" := "dotted, rounded", "shape" := "box" ]
        vertexAttributes UnknownModule{}  = [ "style" := "dotted, rounded", "shape" := "box", "color" := "red", "fontcolor" := "red" ]
        vertexAttributes (Variable n _ s) = [ "label" := T.encodeUtf8Builder (n <> " (Variable)"), "tooltip" := T.encodeUtf8Builder (showSpan s), "style" := "rounded", "shape" := "box" ]
        vertexAttributes (Method   n _ s) = [ "label" := T.encodeUtf8Builder (n <> " (Method)"),   "tooltip" := T.encodeUtf8Builder (showSpan s)  , "style" := "rounded", "shape" := "box" ]
        vertexAttributes (Function n _ s) = [ "label" := T.encodeUtf8Builder (n <> " (Function)"), "tooltip" := T.encodeUtf8Builder (showSpan s), "style" := "rounded", "shape" := "box" ]
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
                 , Member (State (Map (Slot address) ControlFlowVertex)) sig
                 , Member (State (Heap address address value)) sig
                 , Member (State (ScopeGraph address)) sig
                 , Member (Resumable (BaseError (ScopeError address))) sig
                 , Member (Resumable (BaseError (HeapError address))) sig
                 , Member (Reader (CurrentFrame address)) sig
                 , Member (Reader (CurrentScope address)) sig
                 , Member (Reader ControlFlowVertex) sig
                 , VertexDeclaration term
                 , Ord address
                 , Carrier sig m
                 )
              => Open (term Loc -> Evaluator (term Loc) address value m a)
graphingTerms recur term = do
  definedInModule <- currentModule
  case toVertex definedInModule term of
    Just (v@Function{}, name) -> recurWithContext v name
    Just (v@Method{}, name) -> recurWithContext v name
    Just (v@Variable{}, name) -> do
      variableDefinition v
      slot <- lookupSlot (Declaration name)
      defined <- gets (Map.lookup slot)
      maybe (pure ()) (appendGraph . connect (vertex v) . vertex) defined
      recur term
    _ -> recur term
  where
    recurWithContext v name = do
      variableDefinition v
      moduleInclusion v
      local (const v) $ do
        valRef <- recur term
        slot <- lookupSlot (Declaration name)
        modify (Map.insert slot v)
        pure valRef

-- | Add vertices to the graph for evaluated modules and the packages containing them.
graphingPackages :: ( Member (Reader PackageInfo) sig
                    , Member (State (Graph ControlFlowVertex)) sig
                    , Member (Reader ControlFlowVertex) sig
                    , Carrier sig m
                    )
                 => Open (Module term -> m a)
graphingPackages recur m =
  let v = moduleVertex (moduleInfo m) in packageInclusion v *> local (const v) (recur m)

-- | Add vertices to the graph for imported modules.
graphingModules :: ( Member (Reader ModuleInfo) sig
                   , Member (State (Graph ControlFlowVertex)) sig
                   , Member (Reader ControlFlowVertex) sig
                   , Carrier sig m
                   )
                => (Module body -> Evaluator term address value (EavesdropC address value m) a)
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
    includeModule path
      = let path' = if Prologue.null path then "unknown, concrete semantics required" else path
            info = moduleInfo m
      in moduleInclusion (moduleVertex (ModuleInfo path' (moduleLanguage info) (moduleOid info)))

-- | Add vertices to the graph for imported modules.
graphingModuleInfo :: ( Member (Reader ModuleInfo) sig
                      , Member (State (Graph ModuleInfo)) sig
                      , Carrier sig m
                      )
                   => (Module body -> Evaluator term address value (EavesdropC address value m) a)
                   -> (Module body -> Evaluator term address value m a)
graphingModuleInfo recur m = do
  let info = moduleInfo m
  appendGraph (vertex info)
  eavesdrop (recur m) $ \case
    Load   path _ -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path (moduleLanguage info) (moduleOid info))) . vertex
    Lookup path _ -> currentModule >>= appendGraph . (`connect` vertex (ModuleInfo path (moduleLanguage info) (moduleOid info))) . vertex
    _             -> pure ()

eavesdrop :: Evaluator term address value (EavesdropC address value m) a
          -> (forall x . Modules address value m x -> Evaluator term address value m ())
          -> Evaluator term address value m a
eavesdrop m f = raiseHandler (runEavesdropC (runEvaluator . f)) m

newtype EavesdropC address value m a = EavesdropC ((forall x . Modules address value m x -> m ()) -> m a)
  deriving (Alternative, Applicative, Functor, Monad) via (ReaderC (forall x . Modules address value m x -> m ()) m)

runEavesdropC :: (forall x . Modules address value m x -> m ()) -> EavesdropC address value m a -> m a
runEavesdropC f (EavesdropC m) = m f

instance (Carrier sig m, Member (Modules address value) sig, Applicative m) => Carrier sig (EavesdropC address value m) where
  eff op
    | Just eff <- prj op = EavesdropC (\ handler -> let eff' = hmap (runEavesdropC handler) eff in handler eff' *> send eff')
    | otherwise          = EavesdropC (\ handler -> eff (hmap (runEavesdropC handler) op))

-- | Add an edge from the current package to the passed vertex.
packageInclusion :: ( Member (Reader PackageInfo) sig
                    , Member (State (Graph ControlFlowVertex)) sig
                    , Carrier sig m
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
                      )
                   => ControlFlowVertex
                   -> m ()
variableDefinition var = do
  context <- ask
  appendGraph (vertex context `connect` vertex var)

appendGraph :: (Member (State (Graph v)) sig, Carrier sig m) => Graph v -> m ()
appendGraph = modify . (<>)


graphing :: Carrier sig m
         => Evaluator term address value (StateC (Map (Slot address) ControlFlowVertex)
                                         (StateC (Graph ControlFlowVertex)
                                         m)) result
         -> Evaluator term address value m (Graph ControlFlowVertex, result)
graphing = raiseHandler $ runState mempty . fmap snd . runState lowerBound
