{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}

module Reprinting.Tokenize
  ( module Data.Reprinting.Token
  , History (..)
  , mark
  , remark
    -- * The Reprinter monad
  , Tokenizer
  , yield
  , control
  , within
  , within'
  , log
  , ignore
  , sep
  , sepTrailing
  , list
  , hash
  , pair
  , imperative
  -- * Tokenize interface
  , Tokenize (..)
  -- * Invocation/results
  , tokenizing
  ) where

import Prelude hiding (fail, log)
import Prologue hiding (hash, Element)

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.History
import Data.List (intersperse)
import Data.Range
import Data.Record
import Data.Reprinting.Token
import Data.Sequence (singleton)
import Data.Source
import Data.Term

-- | The 'Tokenizer' monad represents a context in which 'Control'
-- tokens and 'Element' tokens can be sent to some downstream
-- consumer. Its primary interface is through the 'Tokenize'
-- typeclass.
type Tokenizer = Eff '[Reader RPContext, State RPState, Writer (Seq Token)]

-- | Yield an 'Element' token in a 'Tokenizer' context.
yield :: Element -> Tokenizer ()
yield = tell . singleton . TElement

-- | Yield a 'Control' token in a 'Tokenizer' context.
control :: Control -> Tokenizer ()
control = tell . singleton . TControl

-- | Emit a log message to the token stream. Useful for debugging.
log :: String -> Tokenizer ()
log = control . Log

-- | Emit an Enter for the given context, then run the provided
-- action, then emit a corresponding Exit.
within :: Context -> Tokenizer () -> Tokenizer ()
within c r = control (Enter c) *> r <* control (Exit c)

-- | Like 'within', but adds 'TOpen' and 'TClose' elements around the action.
within' :: Context -> Tokenizer () -> Tokenizer ()
within' c x = within c $ yield TOpen *> x <* yield TClose

-- | Emit a sequence of tokens interspersed with 'TSep'.
sep :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sep = intersperse (yield TSep) . toList

-- | Emit a sequence of tokens each with trailing 'TSep'.
sepTrailing :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sepTrailing = foldr (\x acc -> x : yield TSep : acc) mempty

-- | Emit a sequence of tokens within a 'TList' Context with appropriate 'TOpen',
-- 'TClose' tokens surrounding.
list :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
list = within' TList . sequenceA_ . sep

-- | Emit a sequence of tokens within a 'THash' Context with appropriate
-- 'TOpen', 'TClose' tokens surrounding and interspersing 'TSep'.
hash :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
hash = within' THash . sequenceA_ . sep

-- | Emit key value tokens with a 'TSep' within an TPair Context
pair :: Tokenizer () -> Tokenizer () -> Tokenizer ()
pair k v = within TPair $ k *> yield TSep <* v

-- | Emit a sequence of tokens within an Imperative Context with appropriate
-- 'TOpen', 'TClose' tokens surrounding and interspersing 'TSep'.
imperative :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
imperative = within' Imperative . sequenceA_ . sep

-- | Shortcut for @const (pure ())@, useful for when no action
-- should be taken.
ignore :: a -> Tokenizer ()
ignore = const (pure ())

-- | An instance of the 'Tokenize' typeclass describes how to emit tokens to
-- pretty print the value of the supplied constructor in its AST context.
class (Show1 constr, Traversable constr) => Tokenize constr where
  -- | Should emit control and data tokens.
  tokenize :: FAlgebra constr (Tokenizer ())

-- | Sums of reprintable terms are reprintable.
instance (Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Tokenize fs) => Tokenize (Sum fs) where
  tokenize = apply @Tokenize tokenize

-- | Annotated terms are reprintable and operate in a context derived from the annotation.
instance (HasField fields History, Show (Record fields), Tokenize a) => Tokenize (TermF a (Record fields)) where
  tokenize t = withHistory t (tokenize (termFOut t))

instance Tokenize [] where
  tokenize = imperative

-- | The top-level function. Pass in a 'Source' and a 'Term' and
-- you'll get out a 'Seq' of 'Token's for later processing.
tokenizing :: (Show (Record fields), Tokenize a, HasField fields History) => Source -> Term a (Record fields) -> Seq Token
tokenizing s t = let h = getField (termAnnotation t) in
  run
  . fmap fst
  . runWriter
  . fmap snd
  . runState (RPState 0)
  . runReader (RPContext s h Reprinting)
  $ foldSubterms descend t *> finish

-- Private interfaces

newtype RPState = RPState
  { _cursor   :: Int -- from SYR, used to slice and dice a 'Source' (mutates)
  } deriving (Show, Eq)

setCursor :: Int -> RPState -> RPState
setCursor c s = s { _cursor = c }

data RPContext = RPContext
  { _source  :: Source
  , _history :: History
  , _strategy :: Strategy
  } deriving (Show, Eq)

data Strategy
  = Reprinting
  | PrettyPrinting
    deriving (Eq, Show)

setStrategy :: Strategy -> RPContext -> RPContext
setStrategy s c = c { _strategy = s }

setHistory :: History -> RPContext -> RPContext
setHistory h c = c { _history = h }

chunk :: Source -> Tokenizer ()
chunk = tell . singleton . Chunk

finish :: Tokenizer ()
finish = do
  crs <- gets _cursor
  src <- asks _source
  chunk (dropSource crs src)

withHistory :: (Annotated t (Record fields), HasField fields History) => t -> Tokenizer a -> Tokenizer a
withHistory x = local (setHistory (getField (annotation x)))

withStrategy :: Strategy -> Tokenizer a -> Tokenizer a
withStrategy x = local (setStrategy x)

-- | A subterm algebra inspired by the /Scrap Your Reprinter/ algorithm.
descend :: (Tokenize constr, HasField fields History) => SubtermAlgebra constr (Term a (Record fields)) (Tokenizer ())
descend t = do
  -- log (showsPrec1 0 (() <$ t) "")
  hist <- asks _history
  strat <- asks _strategy
  let into s = withHistory (subterm s) (subtermRef s)
  case (hist, strat) of
    (Unmodified _, _) -> traverse_ into t
    (Refactored _, PrettyPrinting) -> tokenize (fmap into t)
    (Refactored r, Reprinting) -> do
      crs <- gets _cursor
      src <- asks _source
      let delimiter = Range crs (start r)
      log ("slicing: " <> show delimiter)
      chunk (slice delimiter src)
      modify' (setCursor (start r))
      tokenize (fmap (withStrategy PrettyPrinting . into) t)
      modify' (setCursor (end r))
