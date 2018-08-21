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
  , log
  , ignore

  , sep_
  , sepTrailing_
  , surround_
  , list_
  , hash_
  , pair_
  , imperative_

  -- * Tokenize interface
  , Tokenize (..)
  -- * Invocation/results
  , tokenizing
  ) where

import Prelude hiding (fail, log)
import Prologue hiding (Element)

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
import Lens.Micro

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

-- | Emit a sequence of tokens interspersed with 'TSep'.
sep_ :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sep_ = intersperse (yield TSep) . toList

-- | Emit a sequence of tokens each with trailing 'TSep'.
sepTrailing_ :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
sepTrailing_ = traverse_ (\x -> void x *> yield TSep)

-- | Emit a sequence of tokens with appropriate 'TOpen', 'TClose' tokens
-- surrounding.
surround_ :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
surround_ xs = yield TOpen *> void (sequenceA_ xs) *> yield TClose

-- | Emit a sequence of tokens within a 'TList' Context with appropriate 'TOpen',
-- 'TClose' tokens surrounding.
list_ :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
list_ = within TList . surround_ . sep_

-- | Emit a sequence of tokens within an THash Context with appropriate
-- 'TOpen', 'TClose' tokens surrounding and interspersing 'TSep'.
hash_ :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
hash_ = within THash . surround_ . sep_

-- | Emit key value tokens with a 'TSep' within an TPair Context
pair_ :: Tokenizer () -> Tokenizer () -> Tokenizer ()
pair_ k v = within TPair $ void k *> yield TSep *> v

-- | Emit a sequence of tokens within an Imperative Context with appropriate
-- 'TOpen', 'TClose' tokens surrounding and interspersing 'TSep'.
imperative_ :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
imperative_ = within Imperative . surround_ . sep_

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
  tokenize = imperative_

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

cursor :: Lens' RPState Int
cursor = lens _cursor (\s c -> s { _cursor = c })

strategy :: Lens' RPContext Strategy
strategy = lens _strategy (\s t -> s { _strategy = t })

data RPContext = RPContext
  { _source  :: Source
  , _history :: History
  , _strategy :: Strategy
  } deriving (Show, Eq)

data Strategy
  = Reprinting
  | PrettyPrinting
    deriving (Eq, Show)

history :: Lens' RPContext History
history = lens _history (\c h -> c { _history = h })

chunk :: Source -> Tokenizer ()
chunk = tell . singleton . Chunk

finish :: Tokenizer ()
finish = do
  crs <- gets _cursor
  src <- asks _source
  chunk (dropSource crs src)

withHistory :: (Annotated t (Record fields), HasField fields History) => t -> Tokenizer a -> Tokenizer a
withHistory x = local (set history (getField (annotation x)))

withStrategy :: Strategy -> Tokenizer a -> Tokenizer a
withStrategy x = local (set strategy x)

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
      modify (set cursor (start r))
      tokenize (fmap (withStrategy PrettyPrinting . into) t)
      modify (set cursor (end r))
