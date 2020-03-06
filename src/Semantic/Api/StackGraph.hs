{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.ProtoLens (defMessage)
import           Data.Semilattice.Lower
import           Data.Text (Text, pack)
import qualified Parsing.Parser as Parser
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import qualified Scope.Graph.Convert as Graph
import           Semantic.Api.Bridge
import           Semantic.Task
import           Source.Loc as Loc
import qualified Stack.Graph as Stack

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
  nodes = toSGNode <$> Graph.vertexList (Stack.unGraph graph)
  paths = undefined
  in TempStackGraph { scopeGraphNodes = nodes, scopeGraphPaths = paths }

toSGNode :: Stack.Node -> SGNode
toSGNode node = case node of
  Stack.Declaration s -> SGNode {
      nodeId = 1
    , nodeName = Name.formatName s
    , nodeLine = ""
    , nodeKind = ""
    , nodeSpan = lowerBound
    , Semantic.Api.StackGraph.nodeType = Definition
    }

class ToStackGraph term where
  toStackGraph :: Blob -> term Loc -> TempStackGraph

instance ToStackGraph term where
  -- TODO: Need to produce the graph here
  toStackGraph _ _ = TempStackGraph mempty mempty
