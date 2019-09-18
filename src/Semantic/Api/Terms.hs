{-# LANGUAGE ConstraintKinds, GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.Api.Terms
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
import           Control.Effect.Error
import           Control.Lens
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
import           Data.Term
import qualified Data.Text as T
import qualified Data.Vector as V
import           Parsing.Parser
import           Prologue
import           Rendering.Graph
import           Rendering.JSON hiding (JSON)
import qualified Rendering.JSON
import           Semantic.Api.Bridge
import           Semantic.Proto.SemanticPB hiding (Blob)
import           Semantic.Task
import           Serializing.Format hiding (JSON)
import qualified Serializing.Format as Format
import           Tags.Taggable

termGraph :: (Traversable t, Member Distribute sig, ParseEffects sig m) => t Blob -> m ParseTreeGraphResponse
termGraph blobs = ParseTreeGraphResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: ParseEffects sig m => Blob -> m ParseTreeFileGraph
    go blob = (doParse blob >>= withSomeTerm (pure . render))
      `catchError` \(SomeException e) ->
        pure (ParseTreeFileGraph path lang mempty mempty (V.fromList [ParseError (T.pack (show e))]))
      where
        path = T.pack $ blobPath blob
        lang = bridging # blobLanguage blob

        render :: (Foldable syntax, Functor syntax, ConstructorName syntax) => Term syntax Location -> ParseTreeFileGraph
        render t = let graph = renderTreeGraph t
                       toEdge (Edge (a, b)) = TermEdge (vertexId a) (vertexId b)
                   in ParseTreeFileGraph path lang (V.fromList (vertexList graph)) (V.fromList (fmap toEdge (edgeList graph))) mempty

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
parseTermBuilder TermJSONGraph   = termGraph >=> serialize Format.JSON
parseTermBuilder TermSExpression = distributeFoldMap sexpTerm
parseTermBuilder TermDotGraph    = distributeFoldMap dotGraphTerm
parseTermBuilder TermShow        = distributeFoldMap showTerm
parseTermBuilder TermQuiet       = distributeFoldMap quietTerm

jsonTerm :: (ParseEffects sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonTerm blob = (doParse blob >>= withSomeTerm (pure . renderJSONTerm blob)) `catchError` jsonError blob

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
      in stringUtf8 (status <> "\t" <> show (blobLanguage blob) <> "\t" <> blobPath blob <> "\t" <> show duration <> " ms\n")


type ParseEffects sig m = (Member (Error SomeException) sig, Member Task sig, Carrier sig m)

type TermConstraints =
 '[ Taggable
  , Declarations1
  , ConstructorName
  , HasTextElement
  , Show1
  , ToJSONFields1
  , Traversable
  ]

doParse :: (ParseEffects sig m) => Blob -> m (SomeTerm TermConstraints Location)
doParse blob = case blobLanguage blob of
  Go         -> SomeTerm <$> parse goParser blob
  Haskell    -> SomeTerm <$> parse haskellParser blob
  JavaScript -> SomeTerm <$> parse tsxParser blob
  JSON       -> SomeTerm <$> parse jsonParser blob
  JSX        -> SomeTerm <$> parse tsxParser blob
  Markdown   -> SomeTerm <$> parse markdownParser blob
  Python     -> SomeTerm <$> parse pythonParser blob
  Ruby       -> SomeTerm <$> parse rubyParser blob
  TypeScript -> SomeTerm <$> parse typescriptParser blob
  TSX        -> SomeTerm <$> parse tsxParser blob
  PHP        -> SomeTerm <$> parse phpParser blob
  _          -> noLanguageForBlob (blobPath blob)
