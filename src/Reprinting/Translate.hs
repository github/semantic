{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, OverloadedLists,
             ScopedTypeVariables, TupleSections, TypeFamilyDependencies, TypeApplications, TypeOperators #-}

module Reprinting.Translate
  ( -- Translate (..)
    Translation (..)
  , TranslationException (..)
  , Splice (..)
  , Layout (..)
  , Indent (..)

  , translating
  , translate

  , emit
  , splice
  ) where

import Prologue hiding (Element)
import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Control.Monad.Effect.Writer
import           Data.Language
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import           Data.Sequence (singleton)
import qualified Data.Source as Source


type Translate a = a -> Element -> [Context] -> Translator ()

class Translation (lang :: Language) a where
  translation :: Translate a -> Translate a

type Translator = Eff '[State [Context], Writer (Seq Splice), Exc TranslationException]

translating :: forall lang a . (Translation lang a) => a -> Seq Token -> Either TranslationException (Seq Splice)
translating config tokens
  = run
  . Exc.runError
  . fmap fst
  . runWriter
  . fmap snd
  . runState mempty
  $ traverse_ (translate @lang config) tokens

translate :: forall lang a . (Translation lang a) => a -> Token -> Translator ()
translate config t = case t of
  Chunk source     -> emit (Source.toText source)
  TElement content -> get >>= translation @lang defaultTranslation config content
  TControl ctl     -> case ctl of
    Log _   -> pure mempty
    Enter c -> enterContext c
    Exit c  -> exitContext c

  where
    defaultTranslation :: Translate a
    defaultTranslation _ content context = case (content, context) of
      (Fragment f, _) -> emit f

      (Truth True, _)  -> emit "true"
      (Truth False, _) -> emit "false"
      (Nullity, _)     -> emit "null"

      (Open, List:_)        -> emit "["
      (Open, Associative:_) -> emit "{"

      (Close, List:_)        -> emit "]"
      (Close, Associative:_) -> emit "}"

      (Separator, List:_)        -> emit ","
      (Separator, Associative:_) -> emit ","
      (Separator, Pair:_)        -> emit ":"

      _ -> Exc.throwError (Unexpected "invalid context")

emit :: Text -> Translator ()
emit = tell . splice

enterContext :: Context -> Translator ()
enterContext c = modify' (c :)

exitContext :: Context -> Translator ()
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
