{-# LANGUAGE LambdaCase #-}

module Reprinting.Translate
  ( Translator
  , contextualizing
  ) where

import           Control.Monad
import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Control.Monad.Trans
import           Data.Machine

import           Data.Reprinting.Errors
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import qualified Data.Source as Source

type Translator = Eff '[State [Context], Exc TranslationError]

contextualizing :: ProcessT Translator Token Fragment
contextualizing = repeatedly $ await >>= \case
  Chunk source -> yield . Verbatim . Source.toText $ source
  TElement t -> case t of
    Run f -> lift get >>= \c -> yield (New t c f)
    _     -> lift get >>= yield . Defer t
  TControl ctl -> case ctl of
    Enter c -> enterContext c
    Exit c  -> exitContext c
    _       -> pure ()

enterContext, exitContext :: Context -> PlanT k Fragment Translator ()

enterContext c = lift (modify' (c :))

exitContext c = lift get >>= \case
  (x:xs) -> when (x == c) (lift (modify' (const xs)))
  cs     -> lift (Exc.throwError (UnbalancedPair c cs))
