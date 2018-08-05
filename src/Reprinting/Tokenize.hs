{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, UndecidableInstances #-}

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
  -- * Tokenize interface
  , Tokenize (..)
  -- * Invocation/results
  , tokenizing
  ) where

import Prelude hiding (fail, log)
import Prologue hiding (Element)

import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Sequence (singleton)
import Lens.Micro

import Data.History
import Data.Range
import Data.Record
import Data.Source
import Data.Term
import Data.Reprinting.Token

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

-- | Shortcut for @const (pure ())@, useful for when no action
-- should be taken.
ignore :: a -> Tokenizer ()
ignore = const (pure ())

-- | An instance of the 'Tokenize' typeclass describes how
-- to emit tokens based on the 'History' value of the supplied
-- constructor in its AST context.
class (Show1 constr, Traversable constr) => Tokenize constr where
  -- | Corresponds to 'Generated'. Should emit control and data tokens.
  whenGenerated :: FAlgebra constr (Tokenizer ())

  -- | Corresponds to 'Refactored'. Should emit control and data tokens.
  -- You can often defined this as 'whenGenerated'.
  whenRefactored :: FAlgebra constr (Tokenizer ())

  -- | Corresponds to 'Modified'. Should emit control tokens only.
  -- Defaults to 'sequenceA_', which is a suitable definition for
  -- nodes with no children.
  whenModified :: FAlgebra constr (Tokenizer ())
  whenModified = sequenceA_

-- | Sums of reprintable terms are reprintable.
instance (Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Tokenize fs) => Tokenize (Sum fs) where
  whenGenerated = apply @Tokenize whenGenerated
  whenRefactored = apply @Tokenize whenRefactored
  whenModified = apply @Tokenize whenModified

-- | Annotated terms are reprintable and operate in a context derived from the annotation.
instance (HasField fields History, Show (Record fields), Tokenize a) => Tokenize (TermF a (Record fields)) where
  whenGenerated t = into t (whenGenerated (termFOut t))
  whenRefactored t = into t (whenRefactored (termFOut t))
  whenModified t = into t (whenModified (termFOut t))

-- | The top-level function. Pass in a 'Source' and a 'Term' and
-- you'll get out a 'Seq' of 'Token's for later processing.
tokenizing :: (Show (Record fields), Tokenize a, HasField fields History) => Source -> Term a (Record fields) -> Seq Token
tokenizing s t = let h = getField (termAnnotation t) in
  run
  . fmap fst
  . runWriter
  . fmap snd
  . runState (RPState 0 Reprinting)
  . runReader (RPContext s h)
  $ foldSubterms descend t *> finish

-- Private interfaces

data RPState = RPState
  { _cursor   :: Int -- from SYR, used to slice and dice a 'Source' (mutates)
  , _strategy :: Strategy
  } deriving (Show, Eq)

cursor :: Lens' RPState Int
cursor = lens _cursor (\s c -> s { _cursor = c})

strategy :: Lens' RPState Strategy
strategy = lens _strategy (\s t -> s { _strategy = t})

data RPContext = RPContext
  { _source  :: Source
  , _history :: History
  } deriving (Show, Eq)

history :: Lens' RPContext History
history = lens _history (\c h -> c { _history = h })

chunk :: Source -> Tokenizer ()
chunk = tell . singleton . Chunk

finish :: Tokenizer ()
finish = do
  crs <- gets _cursor
  src <- asks _source
  chunk (dropSource crs src)

into :: (Annotated t (Record fields), HasField fields History) => t -> Tokenizer a -> Tokenizer a
into x = local (set history (getField (annotation x)))

strategize :: Strategy -> Tokenizer ()
strategize new = do
  strat <- gets _strategy
  when (strat /= new) $ do
    control (Change new)
    modify' (set strategy new)

-- | A subterm algebra that implements the /Scrap Your Tokenizer/
-- algorithm.  Whereas /SYR/ uses a zipper to do a top-down
-- depth-first rightward traversal of a tree, we use a subterm algebra
-- over a monad, which performs the same task.
descend :: (Tokenize constr, HasField fields History) => SubtermAlgebra constr (Term a (Record fields)) (Tokenizer ())
descend t = do
  let into' s = into (subterm s) (subtermRef s)
  log (showsPrec1 0 (() <$ t) "")
  h <- asks _history
  let next = fmap into' t
  case h of
    Pristine _ -> pure ()
    Generated -> do
      strategize PrettyPrinting
      whenGenerated next
      strategize Reprinting

    Modified _ -> whenModified next
    Refactored r -> do
      crs <- gets _cursor
      src <- asks _source
      let delimiter = Range crs (start r)
      log ("slicing: " <> show delimiter)
      chunk (slice delimiter src)
      modify (set cursor (start r))
      strategize PrettyPrinting
      whenRefactored next
      strategize Reprinting
      modify (set cursor (end r))
