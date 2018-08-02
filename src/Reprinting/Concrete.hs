{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving,
             KindSignatures, LambdaCase, ScopedTypeVariables,
             TupleSections, TypeFamilyDependencies, TypeOperators #-}

module Reprinting.Concrete
  ( Concrete (..)
  , ConcreteException (..)
  , concretize
  ) where

import Prologue hiding (Element)

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Control.Monad.Effect.Writer
import           Data.Text.Prettyprint.Doc

import           Data.Language
import qualified Data.Source as Source
import           Reprinting.Token

-- | Once the 'Reprintable' algebra has yielded a sequence of tokens,
-- we need to apply per-language interpretation to each token so as to
-- yield language-specific chunks of source code. The 'Concrete'
-- typeclass describes how a given 'Language' interprets tokens, using
-- a stack machine described by the @stack@ parameter.  @stack@ must
-- have a 'Lower' instance so we know where to start.
--
-- Some possible issues we should tackle before finalizing this design:
-- * Is a stack machine too inexpressive?
-- * Is this interface too clumsy? Do we just want to use Eff, or another monad?
-- * Do we want to use a generic MonadError rather than instantiate that to Either?
class Concrete (lang :: Language) where

  type Stack lang = s | s -> lang

  -- | Each 'Element' data token should emit a chunk of source code,
  -- taking into account (but not changing) the state of the stack.
  onElement :: Element -> Stack lang -> Either ConcreteException (Doc a)

  -- | Each 'Control' token can (but doesn't have to) change the state of the stack.
  onControl :: Control -> Stack lang -> Either ConcreteException (Stack lang)

-- | Represents failure occurring in a 'Concrete' machine.
data ConcreteException
  = InvalidContext (Maybe Context) Context [Context]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | Unexpected String
  -- ^ Catch-all exception for unexpected tokens.
    deriving (Eq, Show)

-- | Run a 'Concrete' machine over each 'Token' in the provided
-- 'Sequence'.  Each resulting 'Doc' will be concatenated with
-- 'mconcat'.  Pass in an appropriately-kinded 'Proxy' to select how
-- to interpret the language.
concretize :: (Concrete lang, Lower (Stack lang))
           => Proxy lang -> Seq Token -> Either ConcreteException (Doc a)
concretize prox =
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

data Indent = Space | Tab

instance Pretty Indent where
  pretty Space = " "
  pretty Tab   = "\t"

data Layout
  = Hard Int Indent
  | Soft
  | Don't

instance Pretty Layout where
  pretty (Hard times how) = line <> stimes times (pretty how)
  pretty Soft             = softline
  pretty Don't            = mempty

instance Concrete 'JSON where
  type Stack 'JSON = JSONState
  onControl t st = case t of
    StartLayout -> pure (st { needsLayout = True })
    EndLayout   -> pure (st { needsLayout = False })
    Log _ -> pure st
    Enter c -> pure (push c st)
    Exit c  -> do
      let curr = current st
      if curr /= Just c
        then throwError (InvalidContext curr c (contexts st))
        else pure (pop st)

  onElement c st = do

    case c of
      Fragment f -> pure (pretty f)
      Truth t    -> pure (if t then "true" else "false")
      Nullity    -> pure "null"
      Open -> case current st of
        Just List        -> pure "["
        Just Associative -> pure "["
        x                -> throwError (Unexpected (show (Open, x)))
      Close -> case current st of
        Just List        -> pure "]"
        Just Associative -> pure "}"
        x                -> throwError (Unexpected (show (Close, x)))
      Separator  -> do
        let should = needsLayout st

        let i = pretty $
              case (current st, should) of
                (_, False)            -> Don't
                (Just List, _)        -> Soft
                (Just Associative, _) -> Hard 4 Space
                _                     -> Don't
        case current st of
          Just List        -> pure ("," <> i)
          Just Associative -> pure ("," <> i)
          Just Pair        -> pure ":"
          Nothing          -> pure mempty
          ctx              -> throwError (Unexpected (show ctx))

-- Distribute 'onControl' and 'onElement' over 'Token', using the
-- obvious case to handle 'Chunk' tokens.
step :: Concrete lang => Token -> Stack lang -> Either ConcreteException (Doc a, Stack lang)
step t st = case t of
  Chunk src   -> pure (pretty . Source.toText $ src, st)
  TElement el -> onElement el st >>= \doc -> pure (doc, st)
  TControl ct -> (mempty, ) <$> onControl ct st

-- Kludgy hack to convert 'step' into an effect.
stepM :: forall lang a . Concrete lang => Proxy lang -> Token -> Eff '[Writer (Doc a), State (Stack lang), Exc ConcreteException] ()
stepM _ t = do
  st <- get @(Stack lang)
  case step t st of
    Left exc                 -> Exc.throwError exc
    Right (doc :: Doc a, st) -> tell doc *> put st
