{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, OverloadedLists,
             ScopedTypeVariables, TupleSections, TypeFamilyDependencies, TypeApplications, TypeOperators #-}

module Reprinting.Translate
  ( TranslationException (..)
  , TranslatingEffs
  , Splice (..)
  , Layout (..)
  , Indent (..)
  , translating
  , splice
  ) where

import Prologue hiding (Element)

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Control.Rule
import           Data.Language
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import qualified Data.Source as Source
import Data.Machine
import           Control.Arrow


type TranslatingEffs = '[State [Context], Exc TranslationException]


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
    TElement el  -> get >>= translate el . listToMaybe
    TControl ctl -> case ctl of
      Log _   -> pure mempty
      Enter c -> enterContext c *> pure mempty
      Exit c  -> exitContext c *> pure mempty

  translate el c = let emit = pure . splice el c in case (el, c) of
    (Fragment f, _) -> emit f

    (Truth True, _)  -> emit "true"
    (Truth False, _) -> emit "false"
    (Nullity, _)     -> emit "null"

    (Open, Just List)        -> emit "["
    (Open, Just Associative) -> emit "{"

    (Close, Just List)        -> emit "]"
    (Close, Just Associative) -> emit "}"

    (Separator, Just List)        -> emit ","
    (Separator, Just Associative) -> emit ","
    (Separator, Just Pair)        -> emit ":"

    -- TODO: Maybe put an error token in the stream instead?
    _ -> Exc.throwError (Unexpected "don't know how to translate")

enterContext :: (Member (State [Context]) effs) => Context -> Eff effs ()
enterContext c = modify' (c :)

exitContext :: (Member (State [Context]) effs, Member (Exc TranslationException) effs) => Context -> Eff effs ()
exitContext c = do
  current <- get
  case current of
    (x:xs) | x == c -> modify' (const xs)
    _ -> Exc.throwError (Unexpected "invalid context")

-- | Represents failure occurring in a 'Concrete' machine.
data TranslationException
  = InvalidContext (Maybe Context) Context [Context]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | Unexpected String
  -- ^ Catch-all exception for unexpected tokens.
    deriving (Eq, Show)
