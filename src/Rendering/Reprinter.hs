{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, UndecidableInstances #-}

module Rendering.Reprinter
  ( History (..)
  , mark
  -- * The Reprinter monad
  , Reprinter
  , yield
  , control
  -- * Token types
  , Element (..)
  , Control (..)
  , Context (..)
  -- * Reprintable interface
  , Reprintable (..)
  -- * Invocation/results
  , reprint
  , Token (..)
  ) where

import Prelude hiding (fail)
import Prologue hiding (Element)

import Control.Monad.Effect
import Control.Monad.Effect.State (get, put, runState)
import Control.Monad.Effect.Writer
import Data.Sequence (singleton)

import Data.Range
import Data.Record
import Data.Source
import Data.Term

-- | 'History' values, when attached to a given 'Term', describe the ways in which
-- that term was refactored, if any.
data History
  = Generated         -- ^ A 'Generated' node was created by a refactor and has no position information.
  | Refactored Range  -- ^ A 'Refactored' node was changed by a refactor but still has (possibly-inaccurate) position information.
  | Modified Range    -- ^ A 'Modified' node was not changed by a refactor, but its children may be 'Generated' or 'Refactored'.
  | Pristine Range    -- ^ A 'Pristine' node was not changed and has no changed (non-'Pristine') children.
  deriving (Show, Eq)

-- | Convert a 'Term' annotated with a 'Range' to one annotated with a 'History'.
mark :: Functor f => (Range -> History) -> f (Record (Range ': fields)) -> f (Record (History ': fields))
mark f = fmap go where go (r :. a) = f r :. a

-- | The 'Reprinter' monad represents a context in which 'Control'
-- tokens and 'Element' tokens can be sent to some downstream
-- consumer. Its primary interface is through the 'Reprintable'
-- typeclass.
data Reprinter a where
  Pure :: a -> Reprinter a
  Bind :: Reprinter a -> (a -> Reprinter b) -> Reprinter b

  YElement :: Element -> Reprinter ()
  YControl :: Control -> Reprinter ()
  YChunk   :: Source  -> Reprinter ()
  Finish :: Reprinter ()

  Get :: Reprinter RPState
  Put :: RPState -> Reprinter ()

-- We could implement these types more efficiently, or perhaps move to Freer.
instance Functor Reprinter where
  fmap = liftA

instance Applicative Reprinter where
  pure  = Pure
  (<*>) = ap

instance Monad Reprinter where
  (>>=) = Bind

-- | 'Element' tokens describe atomic pieces of source code to be
-- output to a rendered document. These tokens are language-agnostic
-- and are interpreted into language-specific representations at a
-- later point in the reprinting pipeline.
data Element
  = Fragment Text -- ^ A literal chunk of text.
  | Truth Bool    -- ^ A boolean value.
  | Nullity       -- ^ @null@ or @nil@ or some other zero value.
  | Separator     -- ^ Some sort of delimiter, interpreted in some 'Context'.
    deriving (Eq, Show)

-- | Yield an 'Element' token in a 'Reprinter' context.
yield :: Element -> Reprinter ()
yield = YElement

-- | 'Control' tokens describe information about some AST's context.
-- Though these are ultimately rendered as whitespace (or nothing) on
-- the page, they are needed to provide information as to how deeply
-- subsequent entries in the pipeline should indent.
data Control
  = Enter Context
  | Exit Context
    deriving (Eq, Show)

data Context
  = List
  | Associative
  | Pair
  | Infix Operator
    deriving (Show, Eq)

data Operator
  = Add
    deriving (Show, Eq)

-- | Yield a 'Control' token in a 'Reprinter' context.
control :: Control -> Reprinter ()
control = YControl

-- | An instance of the 'Reprintable' typeclass describes how
-- to emit tokens based on the 'History' value of the supplied
-- constructor in its AST context.
class Traversable constr => Reprintable constr where
  -- | Corresponds to 'Generated'. Should emit control and data tokens.
  whenGenerated :: FAlgebra constr (Reprinter ())

  -- | Corresponds to 'Refactored'. Should emit control and data tokens.
  -- If not provided, defaults to 'whenGenerated'.
  whenRefactored :: FAlgebra constr (Reprinter ())
  whenRefactored = whenGenerated

  -- | Corresponds to 'Modified'. Should emit control tokens only.
  whenModified :: FAlgebra constr (Reprinter ())
  whenModified = sequenceA_

-- | Sums of reprintable terms are reprintable.
instance (Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Reprintable fs) => Reprintable (Sum fs) where
  whenGenerated = apply @Reprintable whenGenerated
  whenRefactored = apply @Reprintable whenRefactored
  whenModified = apply @Reprintable whenModified

-- | Annotated terms are reprintable and operate in a context derived from the annotation.
instance (HasField fields History, Reprintable a) => Reprintable (TermF a (Record fields)) where
  whenGenerated t = locally (withAnn (termFAnnotation t)) (whenGenerated (termFOut t) )
  whenRefactored t = locally (withAnn (termFAnnotation t)) (whenRefactored (termFOut t))
  whenModified t = locally (withAnn (termFAnnotation t)) (whenModified (termFOut t))

-- | 'Token' encapsulates 'Element' and 'Control' tokens, as well as sliced
-- portions of the original 'Source' for a given AST.
data Token
  = Chunk Source
  | TElement Element
  | TControl Control
    deriving (Show, Eq)

-- | The top-level function. Pass in a 'Source' and a 'Term' and
-- you'll get out a 'Seq' of 'Token's for later processing.
reprint :: (Reprintable a, HasField fields History) => Source -> Term a (Record fields) -> Seq Token
reprint s t =
  run
  . fmap fst
  . runWriter
  . fmap snd
  . runState (RPState 0 (getField (termAnnotation t)) s)
  . compile
  $ foldSubterms descend t *> Finish

-- Private interfaces

data RPState = RPState
  { rpCursor  :: Int     -- from SYR, used to slice and dice a 'Source' (mutates)
  , rpHistory :: History -- the currently-examined node (locally mutates)
  , rpSource  :: Source  -- the original source text (immutable)
  }

-- Accessors

cursor :: Reprinter Int
cursor = rpCursor <$> Get

source :: Reprinter Source
source = rpSource <$> Get

history :: Reprinter History
history = rpHistory <$> Get

-- Like 'local', but hand-rolled. That's how you know it's good.
locally :: (RPState -> RPState) -> Reprinter a -> Reprinter a
locally f x = Get >>= \st -> Put (f st) *> x <* Put st

-- Build a mutator function out of a provided 'History'-containing 'Record'.
withAnn :: HasField fields History => Record fields -> RPState -> RPState
withAnn ann s = let h = getField ann in s { rpHistory = h }

-- As 'withAnn', but applied to the monad level.
withHistory :: HasField fields History => Subterm (Term syntax (Record fields)) (Reprinter a) -> Reprinter a
withHistory t = locally (withAnn (termAnnotation (subterm t))) (subtermRef t)

-- A subterm algebra that implements the /Scrap Your Reprinter/ algorithm.
descend :: (Reprintable constr, HasField fields History) => SubtermAlgebra constr (Term a (Record fields)) (Reprinter ())
descend t = history >>= \case
  -- No action is necessary for a pristine node.
  Pristine _   -> pure ()
  Generated    -> whenGenerated (fmap subtermRef t) -- Enter children, generating values
  Modified _   -> whenModified (fmap withHistory t) -- Enter children contextually
  Refactored r -> do
    st <- Get
    -- Slice from cursor->lower bound and log it
    let range = Range (rpCursor st) (start r)
    -- Log the sliced chunk
    source >>= (YChunk . slice range)
    Put (st { rpCursor = start r, rpHistory = Generated })
    -- Enter the children, if any, with the refactoring action
    whenRefactored (fmap subtermRef t)
    -- The cursor is now the upper bound
    Put (st { rpCursor = end r})

-- Interpret a Reprinter to a state/writer effect.
compile :: Reprinter a -> Eff '[State RPState, Writer (Seq Token)] a
compile r = case r of
  Pure v     -> pure v
  Bind p f   -> compile p >>= compile . f
  Get        -> get
  Put a      -> put a
  YChunk c   -> tell (singleton (Chunk c))
  YElement e -> tell (singleton (TElement e))
  YControl c -> tell (singleton (TControl c))
  Finish     -> compile (dropSource <$> cursor <*> source) >>= tell . singleton . Chunk
