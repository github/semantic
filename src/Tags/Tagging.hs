{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, OverloadedStrings, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Tags.Tagging
( runTagging
, Tag(..)
, Kind(..)
, IsTaggable
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Control.Effect.State as Eff
import           Data.Abstract.Declarations (Declarations)
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
        "Function" -> Just Function
        "Method"   -> Just Method
        "Class"    -> Just Class
        "Module"   -> Just Module
        "Call"     -> Just Call
        "Send"     -> Just Call -- Ruby’s Send is considered to be a kind of 'Call'
        _          -> Nothing

type ContextToken = (Text, Range)

contextualizing :: ( Member (State [ContextToken]) sig
                   , Carrier sig m
                   )
                => Source.Source
                -> (Text -> Maybe Kind)
                -> Stream (Of Token) m a
                -> Stream (Of Tag) m a
contextualizing source toKind = Streaming.mapMaybeM $ \case
  Enter x r -> Nothing <$ enterScope (x, r)
  Exit  x r -> Nothing <$ exitScope (x, r)
  Iden iden loc docsLiteralRange -> fmap go (get @[ContextToken]) where
    go = \case
      ((x, r):("Context", cr):_) | Just kind <- toKind x -> Just $ Tag iden kind loc (firstLine (slice r)) (Just (slice cr))
      ((x, r):_) | Just kind <- toKind x -> Just $ Tag iden kind loc (firstLine (slice r)) (slice <$> docsLiteralRange)
      _ -> Nothing
  where
    slice = stripEnd . Source.toText . Source.slice source
    firstLine = T.take 180 . fst . breakOn "\n"

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
