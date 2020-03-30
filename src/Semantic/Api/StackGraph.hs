{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Semantic.Api.StackGraph
  ( parseStackGraph
  , TempStackGraph(..)
  , SGNode(..)
  , SGPath(..)
  ) where


import qualified Algebra.Graph as Graph
import qualified Analysis.Name as Name
import qualified Control.Carrier.Sketch.ScopeGraph as ScopeGraph
import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Exception
import           Control.Lens
import           Data.Blob
import           Data.Foldable
import           Data.Int
import           Data.Language
import           Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import           Data.ProtoLens (defMessage)
import           Data.Semilattice.Lower
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text, pack)
import           Data.Traversable
import           Debug.Trace
import qualified Parsing.Parser as Parser
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import qualified Scope.Graph.Convert as Graph
import           Semantic.Api.Bridge
import           Semantic.Task
import           Source.Loc as Loc
import qualified Stack.Graph as Stack
import qualified Stack.Path as Path
import Data.Functor.Tagged
import Control.Monad.ST
import Data.STRef
import Control.Monad (when)

parseStackGraph :: ( Has (Error SomeException) sig m
                   , Effect sig
                   , Has Distribute sig m
                   , Has Parse sig m
                   , Traversable t
                   )
  => t Blob
  -> m StackGraphResponse
parseStackGraph blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList terms
  where
    go :: ( Has (Error SomeException) sig m
          , Effect sig
          , Has Parse sig m
          )
      => Blob
      -> m StackGraphFile
    go blob = catching $ graphToFile . stackGraphToTempStackGraph <$> graphForBlob blob
      where
        catching m = m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobPath blob
        errorFile e = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage')
          & P.nodes .~ mempty
          & P.paths .~ mempty
          & P.errors .~ [defMessage & P.error .~ pack e]

        graphToFile :: TempStackGraph -> StackGraphFile
        graphToFile graph
          = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage')
          & P.nodes .~ fmap nodeToNode (scopeGraphNodes graph)
          & P.paths .~ fmap pathToPath (scopeGraphPaths graph)

        nodeToNode :: SGNode -> StackGraphNode
        nodeToNode node
          = defMessage
          & P.id .~ nodeId node
          & P.name .~ nodeName node
          & P.line .~ nodeLine node
          & P.kind .~ nodeKind node
          & P.maybe'span ?~ converting # nodeSpan node
          & P.nodeType .~ nodeTypeToNodeType (Semantic.Api.StackGraph.nodeType node)

        pathToPath :: SGPath -> StackGraphPath
        pathToPath path
          = defMessage
          & P.startingSymbolStack .~ pathStartingSymbolStack path
          & P.startingScopeStackSize .~ pathStartingScopeStackSize path
          & P.from .~ pathFrom path
          & P.edges .~ pathEdges path
          & P.to .~ pathTo path
          & P.endingScopeStack .~ pathEndingScopeStack path
          & P.endingSymbolStack .~ pathEndingSymbolStack path

        nodeTypeToNodeType :: SGNodeType -> StackGraphNode'NodeType
        nodeTypeToNodeType = \case
          RootScope     -> P.StackGraphNode'ROOT_SCOPE
          JumpToScope   -> P.StackGraphNode'JUMP_TO_SCOPE
          ExportedScope -> P.StackGraphNode'EXPORTED_SCOPE
          Definition    -> P.StackGraphNode'DEFINITION
          Reference     -> P.StackGraphNode'REFERENCE

-- TODO: These are temporary, will replace with proper datatypes from the scope graph work.
data TempStackGraph
  = TempStackGraph
  { scopeGraphNodes :: [SGNode]
  , scopeGraphPaths :: [SGPath]
  }

data SGPath
  = SGPath
  { pathStartingSymbolStack    :: [Text]
  , pathStartingScopeStackSize :: Int64
  , pathFrom                   :: Int64
  , pathEdges                  :: Text
  , pathTo                     :: Int64
  , pathEndingScopeStack       :: [Int64]
  , pathEndingSymbolStack      :: [Text]
  }
  deriving (Eq, Show)

data SGNode
  = SGNode
  { nodeId   :: Int64
  , nodeName :: Text
  , nodeLine :: Text
  , nodeKind :: Text
  , nodeSpan :: Loc.Span
  , nodeType :: SGNodeType
  }
  deriving (Eq, Show)

data SGNodeType = RootScope | JumpToScope | ExportedScope | Definition | Reference
  deriving (Eq, Show)

graphForBlob :: (Effect sig, Has (Error SomeException) sig m, Has Parse sig m) => Blob -> m (Stack.Graph Stack.Node)
graphForBlob blob = parseWith toStackGraphParsers (fmap fst . ScopeGraph.runSketch lowerBound . Graph.scopeGraph) blob
  where
    toStackGraphParsers :: Map Language (Parser.SomeParser Graph.ToScopeGraph Loc)
    toStackGraphParsers = Parser.preciseParsers

stackGraphToTempStackGraph :: Stack.Graph Stack.Node -> TempStackGraph
stackGraphToTempStackGraph graph = let
  nodes = Maybe.catMaybes $ toSGNode <$> Graph.vertexList (Stack.unGraph graph)
  paths = toPaths graph
  in TempStackGraph { scopeGraphNodes = nodes, scopeGraphPaths = paths }

toSGNode :: Stack.Node -> Maybe SGNode
toSGNode node = (traceShow node (case node of
  Stack.Declaration s -> Just $ SGNode {
      nodeId = 1
    , nodeName = Name.formatName s
    , nodeLine = ""
    , nodeKind = ""
    , nodeSpan = lowerBound
    , Semantic.Api.StackGraph.nodeType = Definition
    }
  Stack.Reference s -> Just $ SGNode {
      nodeId = 1
    , nodeName = Name.formatName s
    , nodeLine = ""
    , nodeKind = ""
    , nodeSpan = lowerBound
    , Semantic.Api.StackGraph.nodeType = Reference
    }
  node -> traceShow ("Ignoring Node in toSGNode: " <> show node) Nothing))

toPaths :: Stack.Graph Stack.Node -> [SGPath]
toPaths graph = let
  mainNodes = flip Set.filter (Stack.vertexSet (Stack.tagGraphUniquely graph)) $ isMainNode
  currentPaths = flip Set.map mainNodes $ \taggedNode@(node Stack.:# _) ->
    let
      path = Path.Path { Path.startingNode = taggedNode, Path.endingNode = taggedNode, Path.edges = mempty, Path.startingSymbolStack = mempty, Path.endingSymbolStack = mempty, Path.startingScopeStackSize = Path.Zero, Path.endingScopeStack = mempty}
      referenceNodePath = path { Path.endingSymbolStack = ((Stack.symbol node :: Stack.Symbol) : (Path.endingSymbolStack path)) }
    in
      if isReferenceNode taggedNode then referenceNodePath else path
  in
    toSGPath <$> reducePaths' graph (toList currentPaths)

-- reducePaths :: Foldable t => Stack.Graph Stack.Node -> t Path.Path -> [Path.Path]
-- reducePaths graph paths =
--   foldr (\path newPaths ->
--     let
--       newPaths' = if
--                   then path : newPaths
--                   else newPaths
--     in
--       if Path.completion path == Path.Complete || Path.isPartial path
--       then path : newPaths
--       else
--         let
--           nextEdges = toList $ Set.filter  (\(a, _) -> a == Path.endingNode path) (Stack.edgeSet (Stack.tagGraphUniquely graph))
--         in
--           flip foldMap nextEdges $ \(source, sink) ->
--             if Maybe.isJust (Seq.elemIndexL (Path.Edge source sink "") (Path.edges path))
--             then newPaths
--             else
--               let newPath = path { Path.edges = (Path.Edge source sink "") Seq.<| (Path.edges path) }
--               in
--                 if Path.validity newPath == Path.Valid
--                 then newPath : newPaths
--                 else newPaths
--         ) mempty paths

reducePaths' :: Foldable t => Stack.Graph Stack.Node -> t Path.Path -> [Path.Path]
reducePaths' graph initialPaths = runST $ do
  currentPathsRef <- newSTRef (toList initialPaths)
  pathsRef <- newSTRef []
  go currentPathsRef pathsRef
  readSTRef pathsRef
  where
    go currentPathsRef pathsRef = do
      currentPaths <- readSTRef currentPathsRef
      case currentPaths of
        [] -> do
          modifySTRef' currentPathsRef (const [])
          pure ()
        (currentPath : rest) -> do
          modifySTRef' currentPathsRef (const rest)

          if (Path.completion currentPath == Path.Complete || Path.isPartial currentPath)
          then modifySTRef' pathsRef (currentPath :)
          else do
            let nextEdges = toList $ Set.filter  (\(a, _) -> a == Path.endingNode currentPath) (Stack.edgeSet (Stack.tagGraphUniquely graph))

            for_ nextEdges $ \(a, b) -> do
              if (Path.Edge a b "") `elem` (Path.edges currentPath)
              then pure ()
              else do
                let newPath = currentPath { Path.edges = (Path.edges currentPath) Seq.|> (Path.Edge a b "") }
                when (Path.validity newPath == Path.Valid) $ do
                  modifySTRef' currentPathsRef (newPath :)
          go currentPathsRef pathsRef

isMainNode :: Stack.Tagged Stack.Node -> Bool
isMainNode (node Stack.:# _) = case node of
  Stack.Root{}          -> True
  Stack.ExportedScope{} -> True
  Stack.Reference{}     -> True
  _                     -> False

isReferenceNode :: Stack.Tagged Stack.Node -> Bool
isReferenceNode (node Stack.:# _) = case node of
  Stack.Reference{} -> True
  _                 -> False

toSGPath :: Path.Path -> SGPath
toSGPath Path.Path{..} = SGPath {
    pathStartingSymbolStack = Name.formatName <$> startingSymbolStack
  , pathStartingScopeStackSize = fromIntegral (fromEnum startingScopeStackSize)
  , pathFrom = startingNode ^. identifier
  , pathEdges = foldMap Path.label edges
  , pathTo = endingNode ^. identifier
  , pathEndingScopeStack = endingScopeStack
  , pathEndingSymbolStack = Name.formatName <$> endingSymbolStack
}

class ToStackGraph term where
  toStackGraph :: Blob -> term Loc -> TempStackGraph

instance ToStackGraph term where
  -- TODO: Need to produce the graph here
  toStackGraph _ _ = TempStackGraph mempty mempty
