{-# LANGUAGE AllowAmbiguousTypes, OverloadedLists, ScopedTypeVariables, TypeFamilyDependencies, TypeOperators #-}

module Reprinting.Translate
  ( Translator
  , contextualizing
  ) where

import Prologue hiding (Element)

import           Control.Arrow
import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Data.Machine
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import           Data.Reprinting.Errors
import qualified Data.Source as Source

type Translator = Eff '[State [Context], Exc TranslationError]

-- | Prepare for language specific translation by contextualizing 'Token's to
-- 'Fragment's.
contextualizing ::
  ( Member (State [Context]) effs
  , Member (Exc TranslationError) effs
  )
  => ProcessT (Eff effs) Token Fragment
contextualizing = flattened <~ autoT (Kleisli step) where
  step t = case t of
    Chunk source -> pure $ copy (Source.toText source)
    TElement el  -> toFragment el <$> get
    TControl ctl -> case ctl of
      Log _   -> pure mempty
      Enter c -> enterContext c $> mempty
      Exit c  -> exitContext c $> mempty

  toFragment el cs = case el of
    Fragment f -> insert el cs f
    _          -> defer el cs

  enterContext :: (Member (State [Context]) effs) => Context -> Eff effs ()
  enterContext c = modify' (c :)

  exitContext ::
    ( Member (State [Context]) effs
    , Member (Exc TranslationError) effs
    )
    => Context -> Eff effs ()
  exitContext c = do
    current <- get
    case current of
      (x:xs) | x == c -> modify' (const xs)
      cs              -> Exc.throwError (UnbalancedPair c cs)
