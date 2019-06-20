{-# LANGUAGE LambdaCase #-}

module Reprinting.Translate
  ( contextualizing
  , TranslatorC
  ) where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.State
import           Control.Monad
import           Control.Monad.Trans
import           Streaming
import           Streaming.Prelude (yield)
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
contextualizing s = Streaming.for s $ \case
  Chunk source -> yield . Verbatim . Source.toText $ source
  Element t -> case t of
    Run f -> lift get >>= \c -> yield (New t c f)
    _     -> lift get >>= yield . Defer t
  Control ctl -> case ctl of
    Enter c -> lift (enterScope c)
    Exit c  -> lift (exitScope c)
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
