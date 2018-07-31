{-# LANGUAGE FunctionalDependencies, KindSignatures, LambdaCase, ScopedTypeVariables, TupleSections, TypeOperators #-}

module Reprinting.Concrete
  ( Concrete (..)
  , ConcreteException (..)
  , Precedence (..)
  , concretize
  ) where

import Prelude hiding (foldl)
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

data ConcreteException
  = InvalidContext (Maybe Context) Context [Context]
  | Unexpected String
    deriving (Eq, Show)

class Concrete (l :: Language) stack | l -> stack, stack -> l where
  initial :: stack

  onElement :: Element -> stack -> Either ConcreteException (Doc a)
  onControl :: Control -> stack -> Either ConcreteException stack

data JSState = JSState
  { _currentPrecedence :: Precedence
  , contexts           :: [Context]
  }

class HasContexts state where
  push    :: Context -> state -> state
  pop     :: state -> state
  current :: state -> Maybe Context

instance HasContexts JSState where
  push c s = s { contexts = c : contexts s }

  pop s = s { contexts = drop 1 (contexts s)}

  current = listToMaybe . contexts

data Precedence
  = None
  | Level Int

instance Concrete 'JSON JSState where

  initial = JSState None []

  onControl t st = case t of
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
      Separator  -> case current st of
        Just List        -> pure ","
        Just Associative -> pure ", "
        Just Pair        -> pure ": "
        Nothing          -> pure mempty
        ctx              -> throwError (Unexpected (show ctx))

step :: Concrete lang state => Token -> state -> Either ConcreteException (Doc a, state)
step t st = case t of
  Chunk src   -> pure (pretty . Source.toText $ src, st)
  TElement el -> onElement el st >>= \doc -> pure (doc, st)
  TControl ct -> (mempty, ) <$> onControl ct st

stepM :: forall lang state a . Concrete lang state => Proxy lang -> Token -> Eff '[Writer (Doc a), State state, Exc ConcreteException] ()
stepM _ t = do
  st <- get @state
  case step t st of
    Left exc                 -> Exc.throwError exc
    Right (doc :: Doc a, st) -> tell doc *> put st

concretize :: Concrete lang state => Proxy lang -> Seq Token -> Either ConcreteException (Doc a)
concretize prox =
  run
  . Exc.runError
  . fmap snd
  . runState initial
  . fmap fst
  . runWriter
  . traverse (stepM prox)
