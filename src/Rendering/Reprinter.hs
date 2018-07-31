{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, UndecidableInstances #-}

module Rendering.Reprinter
  ( History (..)
  , historyRange
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
  -- * Invocation/esults
  , reprint
  , Token (..)
  ) where

import Prelude hiding (fail)
import Prologue hiding (Element)

import           Control.Monad.Effect
import           Control.Monad.Effect.Fail
import           Control.Monad.Effect.State (get, put, runState)
import           Control.Monad.Effect.Writer
import Debug.Trace (traceM)
import Data.Sequence (singleton)

import Data.Algebra
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

-- | Safe accessor for a 'History' datum's 'Range'.
historyRange :: History -> Maybe Range
historyRange (Pristine r)   = Just r
historyRange (Modified r)   = Just r
historyRange (Refactored r) = Just r
historyRange Generated      = Nothing

-- | Convert a 'Term' annotated with a 'Range' to one annotated with a 'History'.
mark :: Functor f => (Range -> History) -> f (Record (Range ': fields)) -> f (Record (History ': fields))
mark f = fmap go where go (r :. a) = f r :. a


data RPState = RPState
  { rpCursor  :: Int
  , rpHistory :: History
  , rpSource  :: Source
  }

data Reprinter a where
  Pure :: a -> Reprinter a
  Bind :: Reprinter a -> (a -> Reprinter b) -> Reprinter b

  YElement :: Element -> Reprinter ()
  YControl :: Control -> Reprinter ()
  YChunk   :: Source  -> Reprinter ()
  Finish :: Reprinter ()

  Get :: Reprinter RPState
  Put :: RPState -> Reprinter ()

instance Functor Reprinter where
  fmap = liftA

instance Applicative Reprinter where
  pure  = Pure
  (<*>) = ap

instance Monad Reprinter where
  (>>=) = Bind

cursor :: Reprinter Int
cursor = rpCursor <$> Get

source :: Reprinter Source
source = rpSource <$> Get

finish :: Reprinter ()
finish = Finish

history :: Reprinter History
history = rpHistory <$> Get

locally :: (RPState -> RPState) -> Reprinter a -> Reprinter a
locally f x = Get >>= \st -> Put (f st) *> x <* Put st

data Element
  = Fragment Text
  | Truth Bool
  | Nullity
  | Separator
    deriving (Eq, Show)

data Control
  = Enter Context
  | Exit Context
    deriving (Eq, Show)

data Context
  = List
  | Associative
  | Pair
  | Parenthesized
    deriving (Show, Eq)

yield :: Element -> Reprinter ()
yield = YElement

control :: Control -> Reprinter ()
control = YControl

class Traversable constr => Reprintable constr where
  whenGenerated :: FAlgebra constr (Reprinter ())

  whenRefactored :: FAlgebra constr (Reprinter ())
  whenRefactored = whenGenerated

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

withAnn :: HasField fields History => Record fields -> RPState -> RPState
withAnn ann s = let h = getField ann in s { rpHistory = h }

withHistory :: HasField fields History
            => Subterm (Term syntax (Record fields)) (Reprinter a) -> Reprinter a
withHistory t = locally (withAnn (termAnnotation (subterm t))) (subtermRef t)

data Token
  = Chunk Source
  | TElement Element
  | TControl Control
    deriving (Show, Eq)

descend :: (Reprintable constr, HasField fields History) => SubtermAlgebra constr (Term a (Record fields)) (Reprinter ())
descend t = history >>= \case
  Pristine _   -> pure ()
  Modified _   -> whenModified (fmap subtermRef t)
  Generated    -> whenGenerated (fmap withHistory t)
  Refactored r -> do
    st <- Get
    let range = Range (rpCursor st) (start r)
    source >>= (YChunk . slice range)
    Put (st { rpCursor = start r })
    whenRefactored (fmap withHistory t)
    Put (st { rpCursor = end r})

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


reprint :: (Reprintable a, HasField fields History) => Source -> Term a (Record fields) -> Seq Token
reprint s t =
  run
  . fmap fst
  . runWriter
  . fmap snd
  . runState (RPState 0 (getField (termAnnotation t)) s)
  . compile
  $ foldSubterms descend t *> finish

-- data Context
--   = List
--   | Associative
--   | Parenthesized
--     deriving (Show, Eq)

-- data Precedence
--   = None
--   | Level Int
--     deriving (Show, Eq)

-- data Control
--   = Enter Context
--   | Leave Context
--   | Coda
--     deriving (Show, Eq, Ord)

-- data Element
--   = Truthy Bool
--   | Decimal Scientific
--   | Whole Scientific
--   | Chunk Text
--   | Keyword Text
--   | Separator
--     deriving (Show, Eq)

-- data Reprinter a where
--   -- Monad stuff


--   -- Tokenization stuff
--   YieldControl :: Control -> Reprinter ()
--   YieldElement :: Element -> Reprinter ()
--   Slice        :: Range -> Reprinter ()

--   GetState  :: Reprinter ReprintState
--   PutState  :: ReprintState -> Reprinter ()

-- instance Functor Reprinter where
--   fmap = liftA

-- instance Applicative Reprinter where
--   pure  = Pure
--   (<*>) = ap

-- instance Monad Reprinter where
--   (>>=) = Then

-- -- Public interface

-- control :: Control -> Reprinter ()
-- control = YieldControl

-- yield :: Element -> Reprinter
-- yield = YieldElement

-- operator :: Precedence -> Reprinter a -> Reprinter a
-- operator p r = do
--   level <- precedence <$> getState
--   when (level >= p) (control (Enter Parenthesized))
--   locally (\s { precedence = p }) r
--   when (level >= p) (control (Leave Parenthesized))

-- data ReprintState = ReprintState
--   { precedence :: Precedence
--   , cursor :: Int
--   , history :: History
--   , range :: Range
--   , source :: Souce
--   }

-- getState :: Reprinter ReprintState
-- getState = GetState

-- putState :: ReprinterState -> Reprinter ()
-- putState = PutState

-- locally :: (ReprintState -> ReprintState) -> Reprinter a -> Reprinter a
-- locally f x = do
--   st <- getState
--   putState (f st) *> x <* putState st

-- -- Reprintable interface

-- instance Reprintable Integer where
--   whenGenerated i = yield (Whole n)

-- instance Reprintable Array where
--   whenPositioned t = do
--     control (Enter List)
--     sequence_ t
--     control (Exit List)

--   whenGenerated t = do
--     control (Enter List)
--     let children = toList t
--     let withCommas = intersperse (yield Separator) t
--     sequence_ withCommas
--     control (Exit List)

-- class Traversable constr => Reprintable constr where
--   whenGenerated  :: FAlgebra constr (Reprinter ())

--   whenRefactored :: FAlgebra constr (Reprinter ())
--   whenRefactored = whenGenerated

--   whenPositioned :: FAlgebra constr (Reprinter ())
--   whenPositioned = sequence_

-- -- | Sums are Reprintable.
-- instance (Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Reprintable fs) => Reprintable (Sum fs) where
--   whenGenerated = apply @Reprintable whenGenerated
--   whenRefactored = apply @Reprintable whenRefactored
--   whenPositioned = apply @Reprintable whenPositioned

-- -- | Annotated terms are reprintable and operate in a context derived from the annotation
-- instance (HasField fields History, Tokenizable (Sum fs)) => Tokenizable (TermF (Sum fs) (Record fields)) where
--   whenGenerated = locally (withAnn (termFAnnotation t)) (termFOut t)
--   whenRefactored = locally (withAnn (termFAnnotation t)) (termFOut t)
--   whenPositioned = locally (withAnn (termFAnnotation t)) (termFOut t)

-- withAnn :: HasField fields History => HasField fields History => Record fields -> ReprintState -> ReprintState
-- withAnn ann s =
--   let h = getField ann in
--     case historyRange h of
--       Just r -> s { range = r, history = h }
--       Nothing -> s { history = h }

-- reprinting :: (Reprintable constr, HasField fields History) => SubtermAlgebra constr (Term (Sum syntax) (Record fields)) (Reprinter ())
-- reprinting = getModification >>= \case
--   Unmodified{} -> return ()
--   ModifiedChildren -> whenPositioned (fmap enterSubterm t)
--   Generated  -> whenGenerated  (fmap rangedSubterm t)
--   Refactored -> do
--     st <- getState

--     let chunk = Range (cursor st) (start (range st))
--     slice chunk

--     putState (s { cursor = start (range st), history = Raw })
--     whenRefactored (fmap rangedSubterm t)
--     putState (s { cursor = end (range st), history = history st})

-- enterSubterm :: (HasField fields Modification, HasField fields Range)
--              => Subterm (Term (Sum syntax) (Record fields)) (Eff Reprinter ())
--              -> Eff Reprinter ()
-- enterSubterm t = locally (\s -> s { history = subtermAnn t }) (withRange t)

-- rangedSubterm :: HasField fields Range
--               => Subterm (Term (Sum syntax) (Record fields)) (Reprinter ())
--               -> Reprinter ()
-- rangedSubterm t = locally (\s -> s { range = subtermAnn t }) (subtermRef t)

-- subtermAnn :: HasField fields f => Subterm (Term a (Record fields)) -> f
-- subtermAnn t = let ann = termAnnotation (subterm t) in getField ann

-- data Token
--   = TControl Control
--   | TElement Element
--   | TSlice Source

-- compilePlan :: ReprintState -> Reprinter a -> (ReprintState, Plan a Token a)
-- compilePlan s (Pure a)  = (s, pure a)
-- compilePlan s (Then x f)= let (new, plan) = compilePlan s x in plan >>= compilePlan new . f
-- compilePlan s (YieldControl c) = (s, yield (TControl c))
-- compilePlan s (YieldElement c) = (s, yield (TElement c))
-- compilePlan s (Slice r)        = (s, yield (TSlice (slice r (source s))))
-- compilePlan s GetState         = (pure s, s)
-- compilePlan n (PutState n)     = (pure (), n)
-- compilePlan
