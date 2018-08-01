{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, UndecidableInstances #-}

module Reprinting.Algebraic
  ( module Reprinting.Token
  , History (..)
  , mark
  , remark
  -- * Token types
  , Element (..)
  , Control (..)
  , Context (..)
    -- * The Reprinter monad
  , Reprinter
  , yield
  , control
  , within
  , log'
  -- * Reprintable interface
  , Reprintable (..)
  -- * Invocation/results
  , reprint
  , Token (..)
  ) where

import Prelude hiding (fail)
import Prologue hiding (Element)

import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Sequence (singleton)

import Data.Range
import Data.Record
import Data.Source
import Data.Term
import Reprinting.Token

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

remark :: Functor f => (Range -> History) -> f (Record (History ': fields)) -> f (Record (History ': fields))
remark f = fmap go where
  go (r :. a) = x :. a where
    x = case r of
      Generated    -> Generated
      Refactored r -> f r
      Modified r   -> f r
      Pristine r   -> f r

-- | The 'Reprinter' monad represents a context in which 'Control'
-- tokens and 'Element' tokens can be sent to some downstream
-- consumer. Its primary interface is through the 'Reprintable'
-- typeclass.
type Reprinter = Eff '[Reader RPContext, State RPState, Writer (Seq Token)]

-- | Yield an 'Element' token in a 'Reprinter' context.
yield :: Element -> Reprinter ()
yield = tell . singleton . TElement

-- | Yield a 'Control' token in a 'Reprinter' context.
control :: Control -> Reprinter ()
control = tell . singleton . TControl

-- | Emit a log message to the token stream. Useful for debugging.
log' :: String -> Reprinter ()
log' = control . Log

-- | Emit an Enter for the given context, then run the provided
-- action, then emit a corresponding Exit.
within :: Context -> Reprinter () -> Reprinter ()
within c r = control (Enter c) *> r <* control (Exit c)

-- | An instance of the 'Reprintable' typeclass describes how
-- to emit tokens based on the 'History' value of the supplied
-- constructor in its AST context.
class (Show (constr ()), Traversable constr) => Reprintable constr where
  -- | Corresponds to 'Generated'. Should emit control and data tokens.
  whenGenerated :: FAlgebra constr (Reprinter ())

  -- | Corresponds to 'Refactored'. Should emit control and data tokens.
  -- You can often defined this as 'whenGenerated'.
  whenRefactored :: FAlgebra constr (Reprinter ())

  -- | Corresponds to 'Modified'. Should emit control tokens only.
  -- Defaults to 'sequenceA_', which is a suitable definition for
  -- nodes with no children.
  whenModified :: FAlgebra constr (Reprinter ())
  whenModified = sequenceA_

-- | Sums of reprintable terms are reprintable.
instance (Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Reprintable fs) => Reprintable (Sum fs) where
  whenGenerated = apply @Reprintable whenGenerated
  whenRefactored = apply @Reprintable whenRefactored
  whenModified = apply @Reprintable whenModified

-- | Annotated terms are reprintable and operate in a context derived from the annotation.
instance (HasField fields History, Show (Record fields), Reprintable a) => Reprintable (TermF a (Record fields)) where
  whenGenerated t = into t (whenGenerated (termFOut t))
  whenRefactored t = into t (whenRefactored (termFOut t))
  whenModified t = into t (whenModified (termFOut t))

-- | The top-level function. Pass in a 'Source' and a 'Term' and
-- you'll get out a 'Seq' of 'Token's for later processing.
reprint :: (Show (Record fields), Reprintable a, HasField fields History) => Source -> Term a (Record fields) -> Seq Token
reprint s t = let h = getField (termAnnotation t) in
  run
  . fmap fst
  . runWriter
  . fmap snd
  . runState (RPState 0)
  . runReader (RPContext s h)
  . compile
  $ foldSubterms descend t *> finish

-- Private interfaces

newtype RPState = RPState
  { rpCursor  :: Int     -- from SYR, used to slice and dice a 'Source' (mutates)
  } deriving (Show, Eq)

data RPContext = RPContext
  { rcSource :: Source
  , rcHistory :: History
  } deriving (Show, Eq)
-- Accessors

cursor :: Reprinter Int
cursor = rpCursor <$> get

source :: Reprinter Source
source = rcSource <$> ask

history :: Reprinter History
history = rcHistory <$> ask

chunk :: Source -> Reprinter ()
chunk = tell . singleton . Chunk

finish :: Reprinter ()
finish = do
  crs <- cursor
  src <- source
  chunk (dropSource crs src)

into :: (Annotated t (Record fields), HasField fields History) => t -> Reprinter a -> Reprinter a
into x = local (\c -> c { rcHistory = getField (annotation x)} )


-- | A subterm algebra that implements the /Scrap Your Reprinter/
-- algorithm.  Whereas /SYR/ uses a zipper to do a top-down
-- depth-first rightward traversal of a tree, we use a subterm algebra
-- over a monad, which performs the same task.
descend :: (Reprintable constr, HasField fields History) => SubtermAlgebra constr (Term a (Record fields)) (Reprinter ())
descend t = do
  let into' s = into (subterm s) (subtermRef s)
  log' (show (() <$ t))

  h <- history
  log' (show h)
  let next = fmap into' t
  case h of
    Pristine _ -> pure ()
    Generated -> whenGenerated next
    Modified _ -> whenModified next
    Refactored r -> do
      crs <- cursor
      src <- source
      log' (show (crs, start r))
      chunk (slice (Range crs (start r)) src)
      put (RPState (start r))
      whenRefactored next
      put (RPState (end r))

  -- traverse_ into' t
-- descend t = history >>= \case
--   -- No action is necessary for a pristine node.
--   Pristine _   -> pure ()
--   Generated    -> whenGenerated (fmap subtermRef t)
--   Modified _   -> whenGenerated (fmap (\x -> into (subterm x) (subtermRef x)) t)
--   Refactored r -> do
--     st <- get @RPState
--     control (Log ("Refactor state is " <> show st))
--     let range = Range (rpCursor st) (start r)
--     source >>= (chunk . slice range)
--     modify' (\s -> s { rpCursor = start r })
--     whenRefactored (fmap (\x -> into (subterm x) (subtermRef x)) t)
--     modify' (\s -> s { rpCursor = end r })

-- Interpret a Reprinter to a state/writer effect.
compile :: Reprinter a -> Reprinter a
compile = id
