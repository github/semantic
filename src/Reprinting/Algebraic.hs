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

import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Sequence (singleton)

import Data.History
import Data.Range
import Data.Record
import Data.Source
import Data.Term
import Reprinting.Token

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
  . runState (RPState 0 Reprinting)
  . runReader (RPContext s h)
  $ foldSubterms descend t *> finish

-- Private interfaces

data RPState = RPState
  { rpCursor  :: Int     -- from SYR, used to slice and dice a 'Source' (mutates)
  , rcStrategy :: Strategy
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
    Generated -> do
      strat <- gets rcStrategy
      when (strat /= PrettyPrinting) $ do
        control (Change PrettyPrinting)
        modify' (\s -> s { rcStrategy = PrettyPrinting})

      whenGenerated next

      strat' <- gets rcStrategy
      when (strat' == PrettyPrinting) $ do
        control (Change Reprinting)
        modify' (\s -> s { rcStrategy = Reprinting})


    Modified _ -> whenModified next
    Refactored r -> do
      crs <- cursor
      src <- source
      log' (show (crs, start r))
      chunk (slice (Range crs (start r)) src)
      modify (\s -> s { rpCursor = start r })
      whenRefactored next
      modify (\s -> s { rpCursor = end r })
