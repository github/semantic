{-# LANGUAGE LambdaCase #-}

module Reprinting.Translate
  ( contextualizing
  , TranslatorC
  ) where

import           Control.Effect.Error
import           Control.Effect.Pure
import           Control.Effect.State
import           Control.Monad
import           Streaming
import qualified Streaming.Prelude as Streaming

import           Data.Reprinting.Errors
import           Data.Reprinting.Scope
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import qualified Data.Source as Source

type TranslatorC
  = StateC [Scope]
  ( ErrorC TranslationError PureC)

contextualizing :: Stream (Of Token) TranslatorC a
                -> Stream (Of Fragment) TranslatorC a
contextualizing = Streaming.mapMaybeM $ \case
  Chunk source -> pure . Just . Verbatim . Source.toText $ source
  Element t -> Just <$> case t of
    Run f -> get >>= \c -> pure (New t c f)
    _     -> get >>= pure . Defer t
  Control ctl -> Nothing <$ case ctl of
    Enter c -> enterScope c
    Exit c  -> exitScope c
    _       -> pure ()

enterScope :: (Member (State [Scope]) sig, Carrier sig m)
           => Scope
           -> m ()
enterScope c = modify (c :)

exitScope :: ( Member (State [Scope]) sig
             , Member (Error TranslationError) sig
             , Carrier sig m
             )
          => Scope
          -> m ()
exitScope c = get >>= \case
  (x:xs) -> when (x == c) (put xs)
  cs     -> throwError (UnbalancedPair c cs)
