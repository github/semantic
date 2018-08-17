{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, OverloadedLists,
             ScopedTypeVariables, TupleSections, TypeFamilyDependencies, TypeApplications, TypeOperators #-}

module Reprinting.Translate
  ( -- Translate (..)
    Translation (..)
  , TranslationException (..)
  , TranslatorEffs
  , Splice (..)
  , Layout (..)
  , Indent (..)

  , translating
  -- , runTranslatingEffs
  , translatingRule
  , translate

  , emit
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

-- type Translator = Eff '[State [Context], Writer (Seq Splice), Exc TranslationException]
type TranslatorEffs = '[State [Context], Exc TranslationException]

translating :: forall lang a .
  ( Translation lang a )
  => a -> Seq Token -> Either TranslationException (Seq Splice)
translating config tokens = undefined
  -- = run
  -- . Exc.runError
  -- . fmap fst
  -- . runWriter
  -- . fmap snd
  -- . runState (mempty :: [Context])
  -- $ traverse_ (oldtranslate @lang config) tokens

-- runTranslatingEffs :: (Effectful m) => Eff TranslatorEffs () -> m '[] (Either TranslationException (Seq Splice))
-- runTranslatingEffs = undefined
--   = Exc.runError
--   . fmap fst
--   . runWriter
--   . fmap snd
--   . runState (mempty :: [Context])

translatingRule :: forall lang a effs .
  ( Translation lang a
  , Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  ) =>
  a -> Rule effs Token (Seq Splice)
translatingRule config = fromEffect "translating" (translate @lang config)

translate :: forall lang a effs .
  ( Translation lang a
  , Member (State [Context]) effs
  , Member (Exc TranslationException) effs
  ) =>
  a -> Token -> Eff effs (Seq Splice)
translate config t = case t of
  Chunk source     -> pure $ splice (Source.toText source)
  TElement content -> do
    a <- get
    either (Exc.throwError . Unexpected) pure (translation @lang defaultTranslation config content a)
  TControl ctl     -> case ctl of
    Log _   -> pure mempty
    Enter c -> enterContext c *> pure mempty
    Exit c  -> exitContext c *> pure mempty

  where
    defaultTranslation :: Translate a
    defaultTranslation _ content context = undefined -- case (content, context) of
  --     (Fragment f, _) -> emit f
  --
  --     (Truth True, _)  -> emit "true"
  --     (Truth False, _) -> emit "false"
  --     (Nullity, _)     -> emit "null"
  --
  --     (Open, List:_)        -> emit "["
  --     (Open, Associative:_) -> emit "{"
  --
  --     (Close, List:_)        -> emit "]"
  --     (Close, Associative:_) -> emit "}"
  --
  --     (Separator, List:_)        -> emit ","
  --     (Separator, Associative:_) -> emit ","
  --     (Separator, Pair:_)        -> emit ":"
  --
  --     _ -> Exc.throwError (Unexpected "invalid context")

-- translatingRule' :: forall lang a effs .
--   ( Translation lang a
--   , Member (State [Context]) effs
--   , Member (Writer (Seq Splice)) effs
--   , Member (Exc TranslationException) effs
--   ) =>
--   a -> Rule effs (Seq Token) (Seq Splice)
-- translatingRule' config = fromEffect "translating" (translate @lang config)

emit :: (Member (Writer (Seq Splice)) effs) => Text -> Eff effs ()
emit = tell . splice

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
