{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.Terms
  ( parseTermBuilder
  , TermOutputFormat(..)
  ) where


-- import Data.Aeson (ToJSON)
import Control.Effect
import Control.Monad
import Control.Effect.Error
-- import Control.Exception
import Data.Blob
import Data.ByteString.Builder
import Data.Quieterm
import Semantic.API.Parse
import Semantic.Task as Task
import Control.Monad.IO.Class
import Serializing.Format
import Data.Either
import Rendering.Graph
import Rendering.JSON hiding (JSON)
import qualified Rendering.JSON

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
parseTermBuilder TermJSONTree = distributeFoldMap jsonTerm >=> serialize JSON
parseTermBuilder TermJSONGraph = distributeFoldMap jsonGraph >=> serialize JSON
parseTermBuilder TermSExpression = distributeFoldMap sexpTerm
parseTermBuilder TermDotGraph = distributeFoldMap dotGraphTerm
parseTermBuilder TermShow = distributeFoldMap showTerm
parseTermBuilder TermQuiet = distributeFoldMap quietTerm

jsonTerm :: (ParseEffects sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonTerm blob = (doParse blob >>= withSomeTerm (pure . renderJSONTerm blob)) `catchError` jsonError blob

jsonGraph :: (ParseEffects sig m) => Blob -> m (Rendering.JSON.JSON "trees" SomeJSON)
jsonGraph blob = (doParse blob >>= withSomeTerm (pure . renderJSONAdjTerm blob)) `catchError` jsonError blob

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

-- parseTermBuilder format = distributeFoldMap (go format)
--   where
--     go :: (ParseEffects sig m, MonadIO m) => TermOutputFormat -> Blob -> m Builder
--     go TermJSONTree blob    = (doParse blob >>= withSomeTerm (serialize JSON . renderJSONTerm blob)) `catchError` jsonError blob
--     go TermJSONGraph blob   = (doParse blob >>= withSomeTerm (serialize JSON . renderJSONAdjTerm blob . renderTreeGraph)) `catchError` jsonError blob
--     go TermSExpression blob = doParse blob >>= withSomeTerm (serialize (SExpression ByConstructorName))
--     go TermDotGraph blob    = doParse blob >>= withSomeTerm (serialize (DOT (termStyle "terms")) . renderTreeGraph)
--     go TermShow blob        = doParse blob >>= withSomeTerm (serialize Show . quieterm)
--     go TermQuiet blob       = showTiming blob <$> time' ( (doParse blob >>= withSomeTerm (fmap (const (Right ())) . serialize Show . quieterm)) `catchError` timingError )
--
--     jsonError blob (SomeException e) = serialize JSON (renderJSONError blob (show e))
--     timingError (SomeException e) = pure (Left (show e))
--
--     showTiming Blob{..} (res, duration) =
--       let status = if isLeft res then "ERR" else "OK"
--       in stringUtf8 (status <> "\t" <> show blobLanguage <> "\t" <> blobPath <> "\t" <> show duration <> " ms\n")
