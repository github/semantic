{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Tagging
( runTagging
, Tag(..)
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Control.Effect as Eff
import           Control.Effect.State
import           Control.Monad.Trans
import           Data.Text as T hiding (empty)
import           Streaming
import           Streaming.Prelude (yield)
import qualified Streaming.Prelude as Streaming

import           Data.Blob
import           Data.Location
import qualified Data.Source as Source
import           Data.Tag
import           Data.Term
import           Tags.Taggable

runTagging :: (IsTaggable syntax)
           => Blob
           -> [Text]
           -> Term syntax Location
           -> [Tag]
runTagging blob symbolsToSummarize
  = Eff.run
  . evalState @[ContextToken] []
  . Streaming.toList_
  . contextualizing blob symbolsToSummarize
  . tagging blob

type ContextToken = (Text, Maybe Range)

-- PT TODO: fix me as well
contextualizing :: ( Member (State [ContextToken]) sig
                   , Carrier sig m
                   )
                => Blob
                -> [Text]
                -> Stream (Of Token) m a
                -> Stream (Of Tag) m a
contextualizing Blob{..} symbolsToSummarize s = Streaming.for s $ \case
  Enter x r -> lift (enterScope (x, r))
  Exit  x r -> lift (exitScope (x, r))
  Iden iden span docsLiteralRange -> lift (get @[ContextToken]) >>= \case
    ((x, r):("Context", cr):xs) | x `elem` symbolsToSummarize
      -> yield $ Tag iden x span (fmap fst xs) (firstLine (slice r)) (slice cr)
    ((x, r):xs) | x `elem` symbolsToSummarize
      -> yield $ Tag iden x span (fmap fst xs) (firstLine (slice r)) (slice docsLiteralRange)
    _ -> pure ()
  where
    slice = fmap (stripEnd . Source.toText . flip Source.slice blobSource)
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
