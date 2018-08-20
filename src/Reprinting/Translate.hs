{-# LANGUAGE AllowAmbiguousTypes, OverloadedLists, ScopedTypeVariables, TypeFamilyDependencies, TypeOperators #-}

module Reprinting.Translate
  ( TranslationException (..)
  , Translator
  , Splice (..)
  , Layout (..)
  , translating
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

translating ::
  ( Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  )
  => ProcessT (Eff effs) Token Splice
translating = flattened <~ autoT (Kleisli step) where

  step ::
    ( Member (State [Context]) effs
    , Member (Exc TranslationException) effs
    )
    => Token -> Eff effs (Seq Splice)
  step t = case t of
    Chunk source -> pure $ copy (Source.toText source)
    TElement el  -> get >>= translate el
    TControl ctl -> case ctl of
      Log _   -> pure mempty
      Enter c -> enterContext c $> mempty
      Exit c  -> exitContext c $> mempty

  translate el cs = let emit = pure . splice el cs in case (el, listToMaybe cs) of
    (Fragment f, _) -> emit f

    (Truth True, _)  -> emit "True"
    (Truth False, _) -> emit "False"
    (Nullity, _)     -> emit "Null"

    (Open,  Just List)        -> emit "["
    (Close, Just List)        -> emit "]"

    (Open,  Just Associative) -> emit "{"
    (Close, Just Associative) -> emit "}"

    (Separator, Just List)        -> emit ","
    (Separator, Just Associative) -> emit ","
    (Separator, Just Pair)        -> emit ":"

    -- TODO: Maybe put an error token in the stream instead?
    _ -> Exc.throwError (NoTranslation el cs)

enterContext :: (Member (State [Context]) effs) => Context -> Eff effs ()
enterContext c = modify' (c :)

exitContext :: (Member (State [Context]) effs, Member (Exc TranslationException) effs) => Context -> Eff effs ()
exitContext c = do
  current <- get
  case current of
    (x:xs) | x == c -> modify' (const xs)
    cs -> Exc.throwError (InvalidContext c cs)

-- | Represents failure occurring in a 'Concrete' machine.
data TranslationException
  = InvalidContext Context [Context]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | NoTranslation Element [Context]
  -- ^ Thrown if no translation found for a given element.
  | Unexpected String
  -- ^ Catch-all exception for unexpected tokens.
    deriving (Eq, Show)
