{-# LANGUAGE AllowAmbiguousTypes, OverloadedLists, ScopedTypeVariables, TypeFamilyDependencies, TypeOperators #-}

module Reprinting.Translate
  ( Translator
  , translating
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

type Translator = Eff '[State [Context], Exc TranslationException]

translating ::
  ( Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  ) => ProcessT (Eff effs) Token Datum
translating = flattened <~ autoT (Kleisli step) where
  step t = case t of
    Chunk source -> pure $ copy (Source.toText source)
    TElement el  -> get >>= pure . spliceFragments el
    TControl ctl -> case ctl of
      Log _   -> pure mempty
      Enter c -> enterContext c $> mempty
      Exit c  -> exitContext c $> mempty
  spliceFragments el cs = case el of
    Fragment f -> insert el cs f
    _ -> raw el cs

enterContext :: (Member (State [Context]) effs) => Context -> Eff effs ()
enterContext c = modify' (c :)

exitContext ::
  ( Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  ) => Context -> Eff effs ()
exitContext c = do
  current <- get
  case current of
    (x:xs) | x == c -> modify' (const xs)
    cs -> Exc.throwError (InvalidContext c cs)
