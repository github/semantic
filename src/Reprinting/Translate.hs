{-# LANGUAGE AllowAmbiguousTypes, OverloadedLists, ScopedTypeVariables, TypeFamilyDependencies, TypeOperators #-}

module Reprinting.Translate
  ( TranslationException (..)
  , Translator
  , Splice (..)
  , Layout (..)
  , translating
  , raisingUnhandled
  , splice
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
import qualified Data.Source as Source

type Translator = Eff '[State [Context], Exc TranslationException]

translating :: (Member (State [Context]) effs, Member (Exc TranslationException) effs)
  => ProcessT (Eff effs) Token Splice
translating = flattened <~ autoT (Kleisli step) where
  step :: (Member (State [Context]) effs, Member (Exc TranslationException) effs)
    => Token -> Eff effs (Seq Splice)
  step t = case t of
    Chunk source -> pure $ copy (Source.toText source)
    TElement el  -> get >>= spliceFragments el
    TControl ctl -> case ctl of
      Log _   -> pure mempty
      Enter c -> enterContext c $> mempty
      Exit c  -> exitContext c $> mempty
  spliceFragments el cs = case el of
    Fragment f -> pure (splice el cs f)
    _ -> pure (unhandled el cs)

enterContext :: (Member (State [Context]) effs)
  => Context -> Eff effs ()
enterContext c = modify' (c :)

exitContext :: (Member (State [Context]) effs, Member (Exc TranslationException) effs)
  => Context -> Eff effs ()
exitContext c = do
  current <- get
  case current of
    (x:xs) | x == c -> modify' (const xs)
    cs -> Exc.throwError (InvalidContext c cs)

raisingUnhandled :: (Member (Exc TranslationException) effs)
  => ProcessT (Eff effs) Splice Splice
raisingUnhandled = autoT (Kleisli step) where
  step (Unhandled el cs) = Exc.throwError (NoTranslation el cs)
  step s = pure s

-- | Represents failure occurring in a 'Concrete' machine.
data TranslationException
  = InvalidContext Context [Context]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | NoTranslation Element [Context]
  -- ^ Thrown if no translation found for a given element.
  | Unexpected String
  -- ^ Catch-all exception for unexpected tokens.
    deriving (Eq, Show)
