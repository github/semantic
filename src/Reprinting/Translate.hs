{-# LANGUAGE LambdaCase #-}

module Reprinting.Translate
  ( Translator
  , contextualizing
  ) where

import           Control.Monad
import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.State
import           Control.Monad.Trans
import           Data.Machine

import           Data.Reprinting.Errors
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import           Data.Reprinting.Scope
import qualified Data.Source as Source

type Translator
  = Eff (StateC [Scope]
  ( Eff (ErrorC TranslationError
  ( Eff VoidC))))

contextualizing :: ProcessT Translator Token Fragment
contextualizing = repeatedly $ await >>= \case
  Chunk source -> yield . Verbatim . Source.toText $ source
  Element t -> case t of
    Run f -> lift get >>= \c -> yield (New t c f)
    _     -> lift get >>= yield . Defer t
  Control ctl -> case ctl of
    Enter c -> enterScope c
    Exit c  -> exitScope c
    _       -> pure ()

enterScope, exitScope :: Scope -> PlanT k Fragment Translator ()

enterScope c = lift (modify (c :))

exitScope c = lift get >>= \case
  (x:xs) -> when (x == c) (lift (modify (const xs)))
  cs     -> lift (throwError (UnbalancedPair c cs))
