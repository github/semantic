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

  , emit
  , splice
  ) where

import Prologue hiding (Element)

import           Control.Monad.Effect
import           Control.Monad.Effect.Exception (Exc)
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Control.Monad.Effect.Writer
import           Data.Sequence (singleton)
import           Data.String
import           Lens.Micro

import           Data.Language
import           Data.Reprinting.Token
import qualified Data.Source as Source


class Translation (lang :: Language) where
  translation :: (Element -> [Context] -> Translator ()) -> Element -> [Context] -> Translator ()

type Translator = Eff '[State [Context], Writer (Seq Splice), Exc TranslationException]

translating' :: forall lang . (Translation lang) => Seq Token -> Either TranslationException (Seq Splice)
translating' tokens
  = run
  . Exc.runError
  . fmap fst
  . runWriter
  . fmap snd
  . runState mempty
  $ translate @lang tokens

translate :: forall lang . (Translation lang) => Seq Token -> Translator ()
translate = traverse_ step where
  step :: Token -> Translator ()
  step t = case t of
    Chunk source -> emit $ splice (Source.toText source)

    TControl ctl -> case ctl of
      Log _   -> pure mempty
      Enter c -> enterContext c
      Exit c  -> exitContext c

    TElement content -> get >>= translation @lang defaultTranslation content

defaultTranslation :: Element -> [Context] -> Translator ()
defaultTranslation content context = case (content, context) of
  (Fragment f, _)      -> emit $ splice f

  (Open, List:_)         -> emit $ splice "["
  (Open, Associative:_)  -> emit $ splice "{"

  (Close, List:_)        -> emit $ splice "]"
  (Close, Associative:_) -> emit $ splice "}"

  (Separator, List:_)       -> emit $ splice ","
  (Separator, Associative:_) -> emit $ splice ":"
  _ -> Exc.throwError (Unexpected "invalid context")

emit :: Seq Splice -> Translator ()
emit = tell

enterContext :: Context -> Translator ()
enterContext c = modify' (c :)

exitContext :: Context -> Translator ()
exitContext c = do
  current <- get
  case current of
    (x:xs) | x == c -> modify' (const xs)
    _ -> Exc.throwError (Unexpected "invalid context")



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
-- * @Coassignment@ might be a better name
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
  = HardWrap Int Indent
  | SoftWrap
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

newtype JSONState = JSONState
  { _contexts    :: [Context]
  } deriving (Eq, Show)

contexts :: Lens' JSONState [Context]
contexts = lens _contexts (\s cs -> s { _contexts = cs })

current :: JSONState -> Maybe Context
current s = s ^? contexts._head

instance Lower JSONState where
  lowerBound = JSONState []

instance Translate 'JSON where
  type Stack 'JSON = JSONState

  onControl t st = case t of
    Log _   -> pure st
    Enter c -> pure (over contexts (c:) st)
    Exit c  -> let curr = current st in
      if curr /= Just c
        then throwError (InvalidContext curr c (st ^. contexts))
        else pure (over contexts tail st)

  onElement c st = let curr = current st in do
    case c of
      Fragment f -> pure . splice $ f
      Truth t    -> pure . splice $ if t then "true" else "false"
      Nullity    -> pure . splice $ "null"
      Open -> do
        case curr of
          Just List        -> pure . splice $ "["
          Just Associative -> pure ["{", Directive (HardWrap 2 Space)]
          x                -> throwError (Unexpected (show (Open, x)))
      Close -> do
        case curr of
          Just List        -> pure . splice $ "]"
          Just Associative -> pure [Directive (HardWrap 0 Space), "}"]
          x                -> throwError (Unexpected (show (Close, x)))
      Separator  -> do
        let curr = current st

        let i = Directive $
              case curr of
                Just List        -> SoftWrap
                Just Associative -> HardWrap 2 Space
                Just Pair        -> SoftWrap
                _                -> Don't

        case curr of
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
