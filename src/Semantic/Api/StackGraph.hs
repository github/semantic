{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Semantic.Api.StackGraph
  ( parseStackGraph,
    parseStackGraphBuilder,
    testStackGraph,
    TempStackGraph (..),
    SGNode (..),
    SGPath (..),
  )
where

import qualified Algebra.Graph as Graph
import qualified Analysis.File as File
import qualified Analysis.Name as Name
import Control.Carrier.Parse.Measured
import Control.Carrier.Reader
import qualified Control.Carrier.StackGraph as ScopeGraph
import Control.Effect.Error
import Control.Exception
import Control.Lens hiding ((|>))
import Control.Monad.ST
import Data.Blob
import Data.ByteString.Builder
import Data.Flag
import Data.Foldable
import Data.Functor.Tagged
import Data.Int
import Data.Language
import Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import Data.ProtoLens (defMessage)
import Data.STRef
import Data.Semilattice.Lower
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Parsing.Parser as Parser
import Proto.Semantic as P hiding
  ( Blob,
  )
import Proto.Semantic_Fields as P
import Proto.Semantic_JSON ()
import qualified Scope.Graph.Convert as Graph
import Semantic.Api.Bridge
import Semantic.Config as Config
import Semantic.Task
import Semantic.Task.Files
import Serializing.Format (Format)
import Source.Loc as Loc
import qualified Stack.Graph as Stack
import qualified Stack.Path as Path
import qualified System.Path as SystemPath

parseStackGraphBuilder ::
  ( Effect sig,
    Has Distribute sig m,
    Has (Error SomeException) sig m,
    Has Parse sig m,
    Has (Reader Config) sig m,
    Traversable t
  ) =>
  Format StackGraphResponse ->
  t Blob ->
  m Builder
parseStackGraphBuilder format blobs =
  parseStackGraph blobs >>= serialize format

testOptions :: Config.Options
testOptions =
  defaultOptions
    { optionsFailOnWarning = flag FailOnWarning True,
      optionsLogLevel = Nothing
    }

parseStackGraphFilePath ::
  ( Has (Error SomeException) sig m,
    Has Distribute sig m,
    Has Parse sig m,
    Has Files sig m,
    Effect sig
  ) =>
  SystemPath.RelFile ->
  m StackGraphResponse
parseStackGraphFilePath path = readBlob (File.fromPath path) >>= runReader preciseLanguageModes . parseStackGraph . pure @[]

testStackGraph :: SystemPath.RelFile -> IO (Either SomeException StackGraphResponse)
testStackGraph path = withOptions testOptions $ \config logger statter -> do
  let session = TaskSession config "-" False logger statter
  runTask session (runParse (parseStackGraphFilePath path))

parseStackGraph ::
  ( Has (Error SomeException) sig m,
    Effect sig,
    Has Distribute sig m,
    Has Parse sig m,
    Traversable t
  ) =>
  t Blob ->
  m StackGraphResponse
parseStackGraph blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList terms
  where
    go ::
      (Has (Error SomeException) sig m, Effect sig, Has Parse sig m) =>
      Blob ->
      m StackGraphFile
    go blob =
      catching $ graphToFile . stackGraphToTempStackGraph <$> graphForBlob blob
      where
        catching m =
          m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = Text.pack $ blobFilePath blob
        errorFile e =
          defMessage
            & P.path
            .~ blobPath'
            & P.language
            .~ (bridging # blobLanguage')
            & P.nodes
            .~ mempty
            & P.paths
            .~ mempty
            & P.errors
            .~ [defMessage & P.error .~ Text.pack e]
        graphToFile :: TempStackGraph -> StackGraphFile
        graphToFile graph =
          defMessage
            & P.path
            .~ blobPath'
            & P.language
            .~ (bridging # blobLanguage')
            & P.nodes
            .~ fmap nodeToNode (scopeGraphNodes graph)
            & P.paths
            .~ fmap pathToPath (scopeGraphPaths graph)
        nodeToNode :: SGNode -> StackGraphNode
        nodeToNode node =
          defMessage
            & P.id
            .~ nodeId node
            & P.name
            .~ nodeName node
            & P.line
            .~ nodeLine node
            & P.kind
            .~ nodeKind node
            & P.maybe'span
            .~ fmap (converting #) (nodeSpan node) -- converting #? nodeSpan node
            & P.nodeType
            .~ nodeTypeToNodeType (Semantic.Api.StackGraph.nodeType node)
        pathToPath :: SGPath -> StackGraphPath
        pathToPath path =
          defMessage
            & P.startingSymbolStack
            .~ pathStartingSymbolStack path
            & P.startingScopeStackSize
            .~ pathStartingScopeStackSize path
            & P.from
            .~ pathFrom path
            & P.edges
            .~ pathEdges path
            & P.to
            .~ pathTo path
            & P.endingScopeStack
            .~ pathEndingScopeStack path
            & P.endingSymbolStack
            .~ pathEndingSymbolStack path
        nodeTypeToNodeType :: SGNodeType -> StackGraphNode'NodeType
        nodeTypeToNodeType = \case
          RootScope -> P.StackGraphNode'ROOT_SCOPE
          JumpToScope -> P.StackGraphNode'JUMP_TO_SCOPE
          Definition -> P.StackGraphNode'DEFINITION
          Reference -> P.StackGraphNode'REFERENCE
          Scope -> P.StackGraphNode'EXPORTED_SCOPE

-- TODO: These are temporary, will replace with proper datatypes from the scope graph work.
data TempStackGraph = TempStackGraph
  { scopeGraphNodes :: [SGNode],
    scopeGraphPaths :: [SGPath]
  }

data SGPath = SGPath
  { pathStartingSymbolStack :: [Text],
    pathStartingScopeStackSize :: Int64,
    pathFrom :: Int64,
    pathEdges :: Text,
    pathTo :: Int64,
    pathEndingScopeStack :: [Int64],
    pathEndingSymbolStack :: [Text]
  }
  deriving (Eq, Show)

data SGNode = SGNode
  { nodeId :: Int64,
    nodeName :: Text,
    nodeLine :: Text,
    nodeKind :: Text,
    nodeSpan :: Maybe Loc.Span,
    nodeType :: SGNodeType
  }
  deriving (Eq, Show)

data SGNodeType = Scope | RootScope | JumpToScope | Definition | Reference
  deriving (Eq, Show)

graphForBlob ::
  (Effect sig, Has (Error SomeException) sig m, Has Parse sig m) =>
  Blob ->
  m (Stack.Graph Stack.Node)
graphForBlob blob =
  parseWith
    toStackGraphParsers
    (fmap fst . ScopeGraph.runStackGraph lowerBound . Graph.scopeGraph)
    blob
  where
    toStackGraphParsers :: Map Language (Parser.SomeParser Graph.ToScopeGraph Loc)
    toStackGraphParsers = Parser.preciseParsers

stackGraphToTempStackGraph :: Stack.Graph Stack.Node -> TempStackGraph
stackGraphToTempStackGraph graph =
  let graph' = Stack.tagGraphUniquely graph
      nodes =
        Maybe.catMaybes $ toSGNode <$> Graph.vertexList (Stack.unGraph graph')
      paths = toPaths graph
   in TempStackGraph {scopeGraphNodes = nodes, scopeGraphPaths = paths}

toSGNode :: Tagged Stack.Node -> Maybe SGNode
toSGNode (node :# tag) = case node of
  Stack.Declaration symbol kind loc ->
    Just $
      SGNode
        { nodeId = tag,
          nodeName = Name.formatName symbol,
          nodeLine = "",
          nodeKind = Text.pack $ show kind,
          nodeSpan = Just (Loc.span loc),
          Semantic.Api.StackGraph.nodeType = Definition
        }
  Stack.Reference symbol kind loc ->
    Just $
      SGNode
        { nodeId = tag,
          nodeName = Name.formatName symbol,
          nodeLine = "",
          nodeKind = Text.pack $ show kind,
          nodeSpan = Just (Loc.span loc),
          Semantic.Api.StackGraph.nodeType = Reference
        }
  Stack.JumpToScope ->
    Just $
      SGNode
        { nodeId = tag,
          nodeName = Name.formatName "JUMP",
          nodeLine = "",
          nodeKind = "",
          nodeSpan = Nothing,
          Semantic.Api.StackGraph.nodeType = JumpToScope
        }
  Stack.Root symbol ->
    Just $
      SGNode
        { nodeId = tag,
          nodeName = Name.formatName symbol,
          nodeLine = "",
          nodeKind = "",
          nodeSpan = Nothing,
          Semantic.Api.StackGraph.nodeType = RootScope
        }
  Stack.Scope symbol ->
    Just $
      SGNode
        { nodeId = tag,
          nodeName = Name.formatName symbol,
          nodeLine = "",
          nodeKind = "",
          nodeSpan = Nothing,
          Semantic.Api.StackGraph.nodeType = Scope
        }
  _ -> Nothing

toPaths :: Stack.Graph Stack.Node -> [SGPath]
toPaths graph =
  let mainNodes =
        Set.filter
          isMainNode
          (Stack.vertexSet (Stack.tagGraphUniquely graph))
      currentPaths = flip Set.map mainNodes $ \taggedNode@(node Stack.:# _) ->
        let path =
              Path.Path
                { Path.startingNode = taggedNode,
                  Path.endingNode = taggedNode,
                  Path.edges = mempty,
                  Path.startingSymbolStack = mempty,
                  Path.endingSymbolStack = mempty,
                  Path.startingScopeStackSize = Path.Zero,
                  Path.endingScopeStack = mempty
                }
            referenceNodePath =
              path
                { Path.endingSymbolStack =
                    Stack.symbol node
                      <| Path.endingSymbolStack path
                }
         in if isReferenceNode taggedNode then referenceNodePath else path
   in toSGPath <$> reducePaths' graph (toList currentPaths)

reducePaths' ::
  Foldable t => Stack.Graph Stack.Node -> t Path.Path -> [Path.Path]
reducePaths' graph initialPaths = runST $ do
  currentPathsRef <- newSTRef (toList initialPaths)
  pathsRef <- newSTRef []
  graphRef <- newSTRef (Stack.tagGraphUniquely graph)
  go currentPathsRef pathsRef graphRef
  readSTRef pathsRef
  where
    go currentPathsRef pathsRef graphRef = do
      currentPaths <- readSTRef currentPathsRef
      case currentPaths of
        [] -> do
          modifySTRef' currentPathsRef (const [])
          pure ()
        (currentPath : rest) -> do
          modifySTRef' currentPathsRef (const rest)

          if ( Path.completion currentPath
                 == Path.Complete
                 || Path.isPartial currentPath
             )
            && Path.validity currentPath
            == Path.Valid
            then do
              modifySTRef' pathsRef (currentPath :)
            else do
              pure ()
          do
            graph <- readSTRef graphRef
            let nextEdges =
                  toList $
                    Set.filter
                      (\(a, _) -> a == Path.endingNode currentPath)
                      (Stack.edgeSet graph)

            for_ nextEdges $ \(a, b) -> do
              if Maybe.isJust
                ( Path.Edge a b ""
                    `Seq.elemIndexL` Path.edges currentPath
                )
                then do
                  pure ()
                else do
                  let newPath = appendEdge currentPath (Path.Edge a b "")
                  case newPath of
                    Just newPath -> do
                      modifySTRef' currentPathsRef (newPath :)
                    Nothing -> do
                      pure ()

          go currentPathsRef pathsRef graphRef

appendEdge :: Path.Path -> Path.Edge -> Maybe Path.Path
appendEdge path edge@(Path.Edge _ sinkNode _) = runST $ do
  currentPathRef <- newSTRef path
  newPathRef <- newSTRef Nothing
  let node = Path.sinkNode edge
  -- FIXME: Append the new edge to the currentPath
  modifySTRef' currentPathRef $ \path ->
    path
      { Path.edges = Path.edges path |> edge,
        Path.endingNode = sinkNode
      }

  -- 2.
  if isReferenceOrPushSymbol node
    then do
      let (node' :# _) = node
      modifySTRef' currentPathRef $ \path ->
        path
          { Path.endingSymbolStack =
              Stack.symbol node'
                <| Path.endingSymbolStack path
          }
      currentPath <- readSTRef currentPathRef
      writeSTRef newPathRef (Just currentPath)
      readSTRef newPathRef
    else-- 3.

      if isDefinitionOrPopSymbol node
        then do
          let (node' :# _) = node
          -- 3.i.
          if null (Path.endingSymbolStack path)
            then do
              modifySTRef' currentPathRef $ \path ->
                path
                  { Path.startingSymbolStack =
                      (Path.startingSymbolStack path)
                        |> (Stack.symbol node')
                  }
              currentPath <- readSTRef currentPathRef
              writeSTRef newPathRef (Just currentPath)
              readSTRef newPathRef
            else
              if Seq.lookup 0 (Path.endingSymbolStack path)
                /= Just (Stack.symbol node')
                then do
                  -- 3.ii.
                  readSTRef newPathRef
                else do
                  -- 3.iii.
                  modifySTRef' currentPathRef $ \path ->
                    path
                      { Path.endingSymbolStack =
                          Seq.drop
                            1
                            (Path.endingSymbolStack path)
                      }
                  currentPath <- readSTRef currentPathRef
                  writeSTRef newPathRef (Just currentPath)
                  readSTRef newPathRef
        else
          if isPushScope node
            then do
              let (_ :# tag) = node
              modifySTRef' currentPathRef $
                \path ->
                  path
                    { Path.endingScopeStack = tag : (Path.endingScopeStack path)
                    }
              currentPath <- readSTRef currentPathRef
              writeSTRef newPathRef (Just currentPath)
              readSTRef newPathRef
            else
              if isJumpToScope node
                then do
                  -- 5.i.a.
                  if null (Path.endingScopeStack path)
                    then
                      if (Path.startingScopeStackSize path == Path.One)
                        then-- 5.i.a.
                          readSTRef newPathRef
                        else do
                          -- 5.i.b.
                          modifySTRef' currentPathRef $
                            \path -> path {Path.startingScopeStackSize = Path.One}
                          currentPath <- readSTRef currentPathRef
                          writeSTRef newPathRef (Just currentPath)
                          readSTRef newPathRef
                    else do
                      -- 5.ii.a
                      path <- readSTRef currentPathRef
                      let scopeTag = head (Path.endingScopeStack path)
                          scopeIdentifier = Seq.index (Path.endingSymbolStack path) 0
                      modifySTRef' currentPathRef $
                        \path ->
                          path
                            { Path.endingScopeStack =
                                drop
                                  1
                                  (Path.endingScopeStack path)
                            }

                      -- 5.ii.b
                      let jumpEdge =
                            Path.Edge
                              node
                              ((Stack.Scope scopeIdentifier) :# scopeTag)
                              "jump"
                      -- 5.ii.c
                      modifySTRef' currentPathRef $ \path ->
                        path
                          { Path.edges = ((Path.edges path) |> jumpEdge),
                            Path.endingNode = (Path.sinkNode jumpEdge)
                          }
                      currentPath <- readSTRef currentPathRef
                      writeSTRef newPathRef (Just currentPath)
                      readSTRef newPathRef
                else
                  if isIgnoreScope node
                    then-- 6

                      if null (Path.endingScopeStack path)
                        then
                          if (Path.startingScopeStackSize path == Path.One)
                            then-- 5.i.a.
                              readSTRef newPathRef
                            else do
                              -- 5.i.b.
                              modifySTRef' currentPathRef $
                                \path -> path {Path.startingScopeStackSize = Path.One}
                              currentPath <- readSTRef currentPathRef
                              writeSTRef newPathRef (Just currentPath)
                              readSTRef newPathRef
                        else do
                          modifySTRef' currentPathRef $
                            \path ->
                              path
                                { Path.endingScopeStack =
                                    drop
                                      1
                                      (Path.endingScopeStack path)
                                }
                          currentPath <- readSTRef currentPathRef
                          writeSTRef newPathRef (Just currentPath)
                          readSTRef newPathRef
                    else
                      if isScope node
                        then do
                          currentPath <- readSTRef currentPathRef
                          writeSTRef newPathRef (Just currentPath)
                          readSTRef newPathRef
                        else readSTRef newPathRef

isReferenceOrPushSymbol :: Stack.Tagged Stack.Node -> Bool
isReferenceOrPushSymbol (node Stack.:# _) = case node of
  Stack.Reference {} -> True
  Stack.PushSymbol {} -> True
  _ -> False

isPushScope :: Stack.Tagged Stack.Node -> Bool
isPushScope (node Stack.:# _) = case node of
  Stack.PushScope {} -> True
  _ -> False

isScope :: Stack.Tagged Stack.Node -> Bool
isScope (node Stack.:# _) = case node of
  Stack.Scope {} -> True
  _ -> False

isJumpToScope :: Stack.Tagged Stack.Node -> Bool
isJumpToScope (node Stack.:# _) = case node of
  Stack.JumpToScope -> True
  _ -> False

isIgnoreScope :: Stack.Tagged Stack.Node -> Bool
isIgnoreScope (node Stack.:# _) = case node of
  Stack.IgnoreScope {} -> True
  _ -> False

isDefinitionOrPopSymbol :: Stack.Tagged Stack.Node -> Bool
isDefinitionOrPopSymbol (node Stack.:# _) = case node of
  Stack.Declaration {} -> True
  Stack.PopSymbol {} -> True
  _ -> False

isMainNode :: Stack.Tagged Stack.Node -> Bool
isMainNode (node Stack.:# _) = case node of
  Stack.Root _ -> True
  Stack.Scope {} -> True
  Stack.Reference {} -> True
  _ -> False

isReferenceNode :: Stack.Tagged Stack.Node -> Bool
isReferenceNode (node Stack.:# _) = case node of
  Stack.Reference {} -> True
  _ -> False

toSGPath :: Path.Path -> SGPath
toSGPath Path.Path {..} =
  SGPath
    { pathStartingSymbolStack = Name.formatName <$> toList startingSymbolStack,
      pathStartingScopeStackSize = fromIntegral (fromEnum startingScopeStackSize),
      pathFrom = startingNode ^. identifier,
      pathEdges = foldMap Path.label edges,
      pathTo = endingNode ^. identifier,
      pathEndingScopeStack = endingScopeStack,
      pathEndingSymbolStack = Name.formatName <$> toList endingSymbolStack
    }
