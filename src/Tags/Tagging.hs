{-# LANGUAGE GADTs, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Tags.Tagging
( runTagging
, Tag(..)
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Control.Effect.State as Eff
import           Data.Text as T hiding (empty)
import           Streaming
import qualified Streaming.Prelude as Streaming

import           Data.Blob
import           Data.Tag
import           Data.Term
import           Source.Loc
import qualified Source.Source as Source
import           Tags.Taggable

runTagging :: (IsTaggable syntax)
           => Blob
           -> [Text]
           -> Term syntax Loc
           -> [Tag]
runTagging blob symbolsToSummarize
  = Eff.run
  . evalState @[ContextToken] []
  . Streaming.toList_
  . contextualizing blob symbolsToSummarize
  . tagging blob

type ContextToken = (Text, Maybe Range)

contextualizing :: ( Member (State [ContextToken]) sig
                   , Carrier sig m
                   )
                => Blob
                -> [Text]
                -> Stream (Of Token) m a
                -> Stream (Of Tag) m a
contextualizing Blob{..} symbolsToSummarize = Streaming.mapMaybeM $ \case
  Enter x r -> Nothing <$ enterScope (x, r)
  Exit  x r -> Nothing <$ exitScope (x, r)
  Iden iden span docsLiteralRange -> get @[ContextToken] >>= pure . \case
    ((x, r):("Context", cr):xs) | x `elem` symbolsToSummarize
      -> Just $ Tag iden x span (fmap fst xs) (firstLine (slice r)) (slice cr)
    ((x, r):xs) | x `elem` symbolsToSummarize
      -> Just $ Tag iden x span (fmap fst xs) (firstLine (slice r)) (slice docsLiteralRange)
    _ -> Nothing
  where
    slice = fmap (stripEnd . Source.toText . Source.slice blobSource)
    firstLine = fmap (T.take 180 . fst . breakOn "\n")

enterScope, exitScope :: ( Member (State [ContextToken]) sig
                         , Carrier sig m
                         )
                      => ContextToken
                      -> m ()
enterScope c = modify @[ContextToken] (c :)
exitScope c = get @[ContextToken] >>= \case
  x:xs -> when (x == c) (put xs)
  -- If we run out of scopes to match, we've hit a tag balance issue;
  -- just continue onwards.
  []   -> pure ()
