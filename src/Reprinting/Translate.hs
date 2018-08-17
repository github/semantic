{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, OverloadedLists,
             ScopedTypeVariables, TupleSections, TypeFamilyDependencies, TypeApplications, TypeOperators #-}

module Reprinting.Translate
  ( Translate (..)
  , Translation (..)
  , TranslationException (..)
  , Splice (..)
  , Layout (..)
  , Indent (..)
  , translating
  , splice
  ) where

import Prologue hiding (Element)
import Control.Rule
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

type Translate a = a -> Element -> [Context] -> Either String (Seq Splice)

class Translation (lang :: Language) a where
  translation :: Translate a -> Translate a

translating :: forall lang a effs .
  ( Translation lang a
  , Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  ) =>
  a -> Rule effs Token (Seq Splice)
translating config = fromEffect "translating" (step @lang config)

step :: forall lang a effs .
  ( Translation lang a
  , Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  ) =>
  a -> Token -> Eff effs (Seq Splice)
step config t = case t of
  Chunk source     -> pure $ splice (Source.toText source)
  TElement content -> do
    context <- get
    let eitherSlices = translation @lang defaultTranslation config content context
    either (Exc.throwError . Unexpected) pure eitherSlices
  TControl ctl     -> case ctl of
    Log _   -> pure mempty
    Enter c -> enterContext c *> pure mempty
    Exit c  -> exitContext c *> pure mempty

  where
    defaultTranslation :: Translate a
    defaultTranslation _ content context = case (content, context) of
      (Fragment f, _) -> Right $ splice f

      (Truth True, _)  -> Right $ splice "true"
      (Truth False, _) -> Right $ splice "false"
      (Nullity, _)     -> Right $ splice "null"

      (Open, List:_)        -> Right $ splice "["
      (Open, Associative:_) -> Right $ splice "{"

      (Close, List:_)        -> Right $ splice "]"
      (Close, Associative:_) -> Right $ splice "}"

      (Separator, List:_)        -> Right $ splice ","
      (Separator, Associative:_) -> Right $ splice ","
      (Separator, Pair:_)        -> Right $ splice ":"

      _ -> Left "defaulTranslate failed, unknown context"

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
