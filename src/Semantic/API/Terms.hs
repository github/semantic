{-# LANGUAGE ConstraintKinds, GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.Terms
  (
    termGraph
  , parseTermBuilder
  , TermOutputFormat(..)

  , doParse
  , ParseEffects
  , TermConstraints

  , SomeTerm(..)
  , withSomeTerm
  ) where


import           Analysis.ConstructorName (ConstructorName)
import           Control.Effect
import           Control.Effect.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Abstract.Declarations
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Either
import           Data.Graph
import           Data.JSON.Fields
import           Data.Language
import           Data.Location
import           Data.Quieterm
import           Parsing.Parser
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import           Semantic.API.Helpers
import           Semantic.API.Types hiding (Blob)
import qualified Semantic.API.Types as API
import           Semantic.Task
import           Serializing.Format hiding (JSON)
import qualified Serializing.Format as Format
import           Tags.Taggable

termGraph :: (Traversable t, Member Distribute sig, ParseEffects sig m) => t API.Blob -> m ParseTreeGraphResponse
termGraph blobs = distributeFoldMap go (fmap apiBlobToBlob blobs)
  where
    go :: ParseEffects sig m => Blob -> m ParseTreeGraphResponse
    go blob = (doParse blob >>= withSomeTerm (pure . render))
      `catchError` \(SomeException e) ->
        pure (ParseTreeGraphResponse mempty mempty [ParseError (blobPath blob) (show e)])

    render t = let graph = renderTreeGraph t
                   toEdge (Edge (a, b)) = TermEdge (vertexId a) (vertexId b)
               in ParseTreeGraphResponse (vertexList graph) (fmap toEdge (edgeList graph)) mempty

data TermOutputFormat
  = TermJSONTree
  | TermJSONGraph
  | TermSExpression
  | TermDotGraph
  | TermShow
  | TermQuiet
  deriving (Eq, Show)

parseTermBuilder :: (Traversable t, Member Distribute sig, ParseEffects sig m, MonadIO m)
  => TermOutputFormat-> t Blob -> m Builder
parseTermBuilder TermJSONTree    = distributeFoldMap jsonTerm >=> serialize Format.JSON -- NB: Serialize happens at the top level for these two JSON formats to collect results of multiple blobs.
parseTermBuilder TermJSONGraph   = distributeFoldMap jsonGraph >=> serialize Format.JSON -- termGraph >=> serialize Format.JSON
-- TODO: Switch Term Graph output on CLI to new format like this:
-- parseTermBuilder TermJSONGraph   = termGraph >=> serialize Format.JSON
parseTermBuilder TermSExpression = distributeFoldMap sexpTerm
parseTermBuilder TermDotGraph    = distributeFoldMap dotGraphTerm
parseTermBuilder TermShow        = distributeFoldMap showTerm
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

jsonTerm :: (ParseEffects sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonTerm blob = (doParse blob >>= withSomeTerm (pure . renderJSONTerm blob)) `catchError` jsonError blob

jsonGraph :: (ParseEffects sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonGraph blob = (doParse blob >>= withSomeTerm (pure . renderJSONAdjTerm blob . renderTreeGraph)) `catchError` jsonError blob

jsonError :: Applicative m => Blob -> SomeException -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonError blob (SomeException e) = pure $ renderJSONError blob (show e)

sexpTerm :: (ParseEffects sig m) => Blob -> m Builder
sexpTerm = doParse >=> withSomeTerm (serialize (SExpression ByConstructorName))

dotGraphTerm :: (ParseEffects sig m) => Blob -> m Builder
dotGraphTerm = doParse >=> withSomeTerm (serialize (DOT (termStyle "terms")) . renderTreeGraph)

showTerm :: (ParseEffects sig m) => Blob -> m Builder
showTerm = doParse >=> withSomeTerm (serialize Show . quieterm)

quietTerm :: (ParseEffects sig m, MonadIO m) => Blob -> m Builder
quietTerm blob = showTiming blob <$> time' ( (doParse blob >>= withSomeTerm (fmap (const (Right ())) . serialize Show . quieterm)) `catchError` timingError )
  where
    timingError (SomeException e) = pure (Left (show e))
    showTiming Blob{..} (res, duration) =
      let status = if isLeft res then "ERR" else "OK"
      in stringUtf8 (status <> "\t" <> show blobLanguage <> "\t" <> blobPath <> "\t" <> show duration <> " ms\n")


type ParseEffects sig m = (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m)

type TermConstraints =
 '[ Taggable
  , Declarations1
  , ConstructorName
  , HasTextElement
  , ToJSONFields1
  ]

doParse :: (ParseEffects sig m) => Blob -> m (SomeTerm TermConstraints Location)
doParse blob@Blob{..} = case blobLanguage of
  Go         -> SomeTerm <$> parse goParser blob
  Haskell    -> SomeTerm <$> parse haskellParser blob
  Java       -> SomeTerm <$> parse javaParser blob
  JavaScript -> SomeTerm <$> parse typescriptParser blob
  JSON       -> SomeTerm <$> parse jsonParser blob
  JSX        -> SomeTerm <$> parse typescriptParser blob
  Markdown   -> SomeTerm <$> parse markdownParser blob
  Python     -> SomeTerm <$> parse pythonParser blob
  Ruby       -> SomeTerm <$> parse rubyParser blob
  TypeScript -> SomeTerm <$> parse typescriptParser blob
  PHP        -> SomeTerm <$> parse phpParser blob
  _          -> noLanguageForBlob blobPath
