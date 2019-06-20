{-# LANGUAGE LambdaCase #-}

module Reprinting.Translate
  ( Translator
  , contextualizing
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

type Translator
  = StateC [Scope]
  ( ErrorC TranslationError PureC)

contextualizing :: Stream (Of Token) Translator a
                -> Stream (Of Fragment) Translator a
contextualizing s = Streaming.for s $ \case
  Chunk source -> yield . Verbatim . Source.toText $ source
  Element t -> case t of
    Run f -> lift get >>= \c -> yield (New t c f)
    _     -> lift get >>= yield . Defer t
  Control ctl -> case ctl of
    Enter c -> enterScope c
    Exit c  -> exitScope c
    _       -> pure ()

-- PT TODO: this can be nicer
enterScope, exitScope :: Scope -> Stream (Of Fragment) Translator ()
enterScope c = lift (modify (c :))
exitScope c = lift get >>= \case
  (x:xs) -> when (x == c) (lift (modify (const xs)))
  cs     -> lift (throwError (UnbalancedPair c cs))
