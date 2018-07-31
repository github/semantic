{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, KindSignatures, LambdaCase, ScopedTypeVariables, TupleSections, TypeOperators #-}

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
-- * Can we remove this somewhat-warty functional dependency?
class Lower stack => Concrete (l :: Language) stack | l -> stack, stack -> l where

  -- | Each 'Element' data token should emit a chunk of source code,
  -- taking into account (but not changing) the state of the stack.
  onElement :: Element -> stack -> Either ConcreteException (Doc a)

  -- | Each 'Control' token can (but doesn't have to) change the state of the stack.
  onControl :: Control -> stack -> Either ConcreteException stack

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
concretize :: Concrete lang state => Proxy lang -> Seq Token -> Either ConcreteException (Doc a)
concretize prox =
  run
  . Exc.runError
  . fmap snd
  . runState lowerBound
  . fmap fst
  . runWriter
  . traverse (stepM prox)

-- Private interfaces

newtype JSONState = JSONState { contexts :: [Context] }
  deriving (Eq, Show, Lower)

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

instance Concrete 'JSON JSONState where
  onControl t st = case t of
    Log _ -> pure st
    Enter c -> pure (push c st)
    Exit c  -> do
      let curr = current st
      if curr /= Just c
        then throwError (InvalidContext curr c (contexts st))
        else pure (pop st)

  onElement c st =
    case c of
      Fragment f -> pure (pretty f)
      Truth t    -> pure (if t then "true" else "false")
      Nullity    -> pure "null"
      Open -> case current st of
        Just List -> pure "["
        Just Associative -> pure "["
        x -> throwError (Unexpected (show (Open, x)))
      Close -> case current st of
        Just List -> pure "]"
        Just Associative -> pure "}"
        x -> throwError (Unexpected (show (Close, x)))
      Separator  -> case current st of
        Just List        -> pure ","
        Just Associative -> pure ", "
        Just Pair        -> pure ": "
        Nothing          -> pure mempty
        ctx              -> throwError (Unexpected (show ctx))

-- Distribute 'onControl' and 'onElement' over 'Token', using the
-- obvious case to handle 'Chunk' tokens.
step :: Concrete lang state => Token -> state -> Either ConcreteException (Doc a, state)
step t st = case t of
  Chunk src   -> pure (pretty . Source.toText $ src, st)
  TElement el -> onElement el st >>= \doc -> pure (doc, st)
  TControl ct -> (mempty, ) <$> onControl ct st

-- Kludgy hack to convert 'step' into an effect.
stepM :: forall lang state a . Concrete lang state => Proxy lang -> Token -> Eff '[Writer (Doc a), State state, Exc ConcreteException] ()
stepM _ t = do
  st <- get @state
  case step t st of
    Left exc                 -> Exc.throwError exc
    Right (doc :: Doc a, st) -> tell doc *> put st
