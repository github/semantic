{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Semantic.Api.ScopeGraph
  ( parseScopeGraph
  , TempScopeGraph(..)
  , SGNode(..)
  , SGPath(..)
  ) where


import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Exception
import           Control.Lens
import           Data.Blob
import           Data.Int
import           Data.Map.Strict (Map)
import           Data.Language
import           Data.Foldable
import           Data.ProtoLens (defMessage)
import           Semantic.Api.Bridge
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import           Data.Text (Text, pack)
import           Source.Loc as Loc
import           Semantic.Task
import qualified Parsing.Parser as Parser

-- import qualified Language.Go as Go

parseScopeGraph :: ( Has Distribute sig m
                   , Has (Error SomeException) sig m
                   , Has Parse sig m, Traversable t
                   )
  => t Blob
  -> m ParseTreeScopeGraphResponse
parseScopeGraph blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList terms
  where
    go :: (Has (Error SomeException) sig m, Has Parse sig m) => Blob -> m ScopeGraphFile
    go blob = catching $ graphToFile <$> graphForBlob blob
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

        graphToFile :: TempScopeGraph -> ScopeGraphFile
        graphToFile graph
          = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage')
          & P.nodes .~ fmap nodeToNode (scopeGraphNodes graph)
          & P.paths .~ fmap pathToPath (scopeGraphPaths graph)

        nodeToNode :: SGNode -> ScopeGraphNode
        nodeToNode node
          = defMessage
          & P.id .~ nodeId node
          & P.name .~ nodeName node
          & P.line .~ nodeLine node
          & P.syntax .~ nodeSyntax node
          & P.isDefinition .~ nodeIsDefinition node
          & P.maybe'span ?~ converting # nodeSpan node

        pathToPath :: SGPath -> ScopeGraphPath
        pathToPath path
          = defMessage
          & P.startingSymbolStack .~ pathStartingSymbolStack path
          & P.startingScopeStackSize .~ pathStartingScopeStackSize path
          & P.from .~ pathFrom path
          & P.edges .~ pathEdges path
          & P.to .~ pathTo path
          & P.endingScopeStack .~ pathEndingScopeStack path
          & P.endingSymbolStack .~ pathEndingSymbolStack path


-- TODO: These are temporary, will replace with proper datatypes from the scope graph work.
data TempScopeGraph
  = TempScopeGraph
  { scopeGraphNodes :: [SGNode]
  , scopeGraphPaths :: [SGPath]
  }

data SGPath
  = SGPath
  { pathStartingSymbolStack :: Text
  , pathStartingScopeStackSize :: Int32
  , pathFrom :: Int32
  , pathEdges :: Text
  , pathTo :: Int32
  , pathEndingScopeStack :: Text
  , pathEndingSymbolStack :: Text
  }
  deriving (Eq, Show)

data SGNode
  = SGNode
  { nodeId :: Int32
  , nodeName :: Text
  , nodeLine :: Text
  , nodeSyntax :: Text
  , nodeIsDefinition :: Bool
  , nodeSpan :: Loc.Span
  }

graphForBlob :: (Has (Error SomeException) sig m, Has Parse sig m) => Blob -> m TempScopeGraph
graphForBlob blob = parseWith toScopeGraphParsers (pure . toScopeGraph blob) blob
  where
    toScopeGraphParsers :: Map Language (Parser.SomeParser ToScopeGraph Loc)
    toScopeGraphParsers = Parser.preciseParsers

class ToScopeGraph term where
  toScopeGraph :: Blob -> term Loc -> TempScopeGraph

instance ToScopeGraph term where
  -- TODO: Need to produce the graph here
  toScopeGraph _ _ = TempScopeGraph mempty mempty
