{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tags.Tagging
( runTagging
, Tag(..)
, Kind(..)
, IsTaggable
)
where

import Prelude hiding (fail, filter, log)

import           Control.Carrier.State.Strict as Eff
import           Control.Monad
import           Data.Abstract.Declarations (Declarations)
import           Data.Functor.Foldable
import           Data.Text as T hiding (empty)
import           Streaming
import qualified Streaming.Prelude as Streaming

import           Data.Language
import           Data.Term
import           Source.Loc
import qualified Source.Source as Source
import           Tags.Tag
import           Tags.Taggable

runTagging :: (IsTerm term, IsTaggable (Syntax term), Base (term Loc) ~ TermF (Syntax term) Loc, Recursive (term Loc), Declarations (term Loc))
           => Language
           -> [Text]
           -> Source.Source
           -> term Loc
           -> [Tag]
runTagging lang symbolsToSummarize source
  = Eff.run
  . evalState @[ContextToken] []
  . Streaming.toList_
  . contextualizing source toKind
  . tagging lang
  where
    toKind x = do
      guard (x `elem` symbolsToSummarize)
      case x of
        "Function"        -> Just Function
        "Method"          -> Just Method
        "Class"           -> Just Class
        "Module"          -> Just Module
        "Call"            -> Just Call
        "Send"            -> Just Call -- Ruby’s Send is considered to be a kind of 'Call'
        "AmbientFunction" -> Just Function -- Classify TypeScript ambient functions as 'Function'
        _                 -> Nothing

type ContextToken = (Text, Range)

contextualizing :: Has (State [ContextToken]) sig m
                => Source.Source
                -> (Text -> Maybe Kind)
                -> Stream (Of Token) m a
                -> Stream (Of Tag) m a
contextualizing source toKind = Streaming.mapMaybeM $ \case
  Enter x r -> Nothing <$ enterScope (x, r)
  Exit  x r -> Nothing <$ exitScope (x, r)
  Iden iden loc docsLiteralRange -> fmap go (get @[ContextToken]) where
    go = \case
      ((x, r):("Context", cr):_) | Just kind <- toKind x -> Just $ Tag iden kind loc (firstLine r) (Just (sliceDocs cr))
      ((x, r):_) | Just kind <- toKind x -> Just $ Tag iden kind loc (firstLine r) (sliceDocs <$> docsLiteralRange)
      _ -> Nothing
  where
    slice = Source.toText . Source.slice source
    sliceDocs = T.stripEnd . slice
    firstLine = T.stripEnd . T.take 180 . T.takeWhile (/= '\n') . slice

enterScope, exitScope :: Has (State [ContextToken]) sig m
                      => ContextToken
                      -> m ()
enterScope c = modify @[ContextToken] (c :)
exitScope c = get @[ContextToken] >>= \case
  x:xs -> when (x == c) (put xs)
  -- If we run out of scopes to match, we've hit a tag balance issue;
  -- just continue onwards.
  []   -> pure ()
