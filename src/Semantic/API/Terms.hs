{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.Terms
  ( parseTermBuilder
  , TermOutputFormat(..)
  ) where

import Control.Effect
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

data TermOutputFormat
  = TermJSON
  | TermSExpression
  | TermShow
  | TermQuiet
  deriving (Eq, Show)

parseTermBuilder :: (Traversable t, Member Distribute sig, ParseEffects sig m, MonadIO m)
  => TermOutputFormat-> t Blob -> m Builder
parseTermBuilder format = distributeFoldMap (go format)
  where
    go :: (ParseEffects sig m, MonadIO m)
      => TermOutputFormat -> Blob -> m Builder
    go TermJSON blob@Blob{..} = doParse blob >>= withSomeTerm (serialize JSON)
    go TermSExpression blob@Blob{..} = doParse blob >>= withSomeTerm (serialize (SExpression ByConstructorName))
    go TermShow blob = doParse blob >>= withSomeTerm (serialize Show . quieterm)
    go TermQuiet blob = showTiming blob <$> time'
      (
      (doParse blob >>= withSomeTerm (fmap (const (Right ())) . serialize Show . quieterm))
      `catchError` \(SomeException e) -> pure (Left (show e))
      )
      where showTiming Blob{..} (res, duration) =
              let status = if isLeft res then "ERR" else "OK"
              in stringUtf8 (status <> "\t" <> show blobLanguage <> "\t" <> blobPath <> "\t" <> show duration <> " ms\n")
