{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, OverloadedLists,
             ScopedTypeVariables, TupleSections, TypeFamilyDependencies, TypeOperators #-}

module Reprinting.Translate
  ( Translate (..)
  , TranslationException (..)
  , Splice (..)
  , Layout (..)
  , Indent (..)
  , translating
  ) where

import Prologue hiding (Element)

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Control.Monad.Effect.Writer
import           Data.String
import Data.Sequence (singleton)

import           Data.Language
import           Data.Reprinting.Token
import qualified Data.Source as Source

-- | Once the 'Tokenize' algebra has yielded a sequence of tokens,
-- we need to apply per-language interpretation to each token so as to
-- yield language-specific 'Splice's of source code. The 'Translate'
-- typeclass describes a stack machine capable of translating a given
-- a stream of 'Tokens', based on the concrete syntax of the specified
-- language, into concrete-syntax 'Splice's.
--
-- Some possible issues we should tackle before finalizing this design:
--
-- * Is a stack machine too inexpressive?
-- * Is this interface too clumsy? Do we just want to use Eff, or another monad?
-- * Do we want to use a generic MonadError rather than instantiate that to Either?
class Translate (lang :: Language) where

  type Stack lang = s | s -> lang

  -- | Each 'Element' data token should emit a chunk of source code,
  -- taking into account (but not changing) the state of the stack.
  onElement :: Element -> Stack lang -> Either TranslationException (Seq Splice)

  -- | Each 'Control' token can (but doesn't have to) change the state of the stack.
  onControl :: Control -> Stack lang -> Either TranslationException (Stack lang)

-- | Indentation types. This will eventually be moved into the rules engine.
data Indent = Space | Tab deriving (Eq, Show)

-- | Indentation/spacing directives.
data Layout
  = Hard Int Indent
  | Soft
  | Don't
    deriving (Eq, Show)

-- | The simplest possible representation of concrete syntax: either
-- it's a run of literal text or information about whitespace.
data Splice
  = Insert Text
  | Directive Layout
    deriving (Eq, Show)

splice :: Text -> Seq Splice
splice = singleton . Insert

instance IsString Splice where fromString = Insert . fromString

-- | Represents failure occurring in a 'Concrete' machine.
data TranslationException
  = InvalidContext (Maybe Context) Context [Context]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | Unexpected String
  -- ^ Catch-all exception for unexpected tokens.
    deriving (Eq, Show)

-- | Run a 'Concrete' machine over each 'Token' in the provided
-- 'Sequence'.  Each resulting 'Doc' will be concatenated with
-- 'mconcat'.  Pass in an appropriately-kinded 'Proxy' to select how
-- to interpret the language.
translating :: (Translate lang, Lower (Stack lang))
            => Proxy lang
            -> Seq Token
            -> Either TranslationException (Seq Splice)
translating prox =
  run
  . Exc.runError
  . fmap snd
  . runState lowerBound
  . fmap fst
  . runWriter
  . traverse (stepM prox)

-- Private interfaces

data JSONState = JSONState
  { contexts    :: [Context]
  , needsLayout :: Bool
  }
  deriving (Eq, Show)

instance Lower JSONState where
  lowerBound = JSONState [] False

-- A class for pushing and popping contexts. This may or may not be useful
-- when we implement 'Concrete' for languages other than JSON.
class ContextStack state where
  push    :: Context -> state -> state
  pop     :: state -> state
  current :: state -> Maybe Context

instance ContextStack JSONState where
  push c s = s { contexts = c : contexts s }

  pop s = s { contexts = drop 1 (contexts s)}

  current = listToMaybe . contexts

instance Translate 'JSON where
  type Stack 'JSON = JSONState
  onControl t st = case t of
    Change PrettyPrinting -> pure (st { needsLayout = True })
    Change Reprinting     -> pure (st { needsLayout = False })
    Log _ -> pure st
    Enter c -> pure (push c st)
    Exit c  -> do
      let curr = current st
      if curr /= Just c
        then throwError (InvalidContext curr c (contexts st))
        else pure (pop st)

  onElement c st = do

    case c of
      Fragment f -> pure . splice $ f
      Truth t    -> pure . splice $ if t then "true" else "false"
      Nullity    -> pure . splice $ "null"
      Open -> case current st of
        Just List        -> pure . splice $ "["
        Just Associative -> pure . splice $ "["
        x                -> throwError (Unexpected (show (Open, x)))
      Close -> case current st of
        Just List        -> pure . splice $ "]"
        Just Associative -> pure . splice $ "}"
        x                -> throwError (Unexpected (show (Close, x)))
      Separator  -> do
        let should = needsLayout st

        let i = Directive $
              case (current st, should) of
                (_, False)            -> Don't
                (Just List, _)        -> Soft
                (Just Associative, _) -> Hard 4 Space
                (Just Pair, _)        -> Soft
                _                     -> Don't

        case current st of
          Just List        -> pure [",", i]
          Just Associative -> pure [",", i]
          Just Pair        -> pure [":", i]
          Nothing          -> pure mempty
          ctx              -> throwError (Unexpected (show ctx))

-- Distribute 'onControl' and 'onElement' over 'Token', using the
-- obvious case to handle 'Chunk' tokens.
step :: Translate lang => Token -> Stack lang -> Either TranslationException (Seq Splice, Stack lang)
step t st = case t of
  Chunk src   -> pure (splice . Source.toText $ src, st)
  TElement el -> onElement el st >>= \s -> pure (s, st)
  TControl ct -> (mempty, ) <$> onControl ct st

-- Kludgy hack to convert 'step' into an effect.
stepM :: forall lang . Translate lang => Proxy lang -> Token -> Eff '[Writer (Seq Splice), State (Stack lang), Exc TranslationException] ()
stepM _ t = do
  st <- get @(Stack lang)
  case step t st of
    Left exc                    -> Exc.throwError exc
    Right (s :: Seq Splice, st) -> tell s *> put st
