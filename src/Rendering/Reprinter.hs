{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators #-}

module Rendering.Reprinter
  ( History (..)
  , historyRange
  , mark
  -- * The Reprinter monad
  , Reprinter
  , reprint
  , Token (..)
  ) where

import Prologue

import Control.Monad.Effect
import Control.Monad.Effect.State (get, put, runState)
import Control.Monad.Trans (lift)
import Data.Machine hiding (run, source, Source)

import           Data.Algebra
import           Data.Range
import           Data.Record
import Data.Source
import           Data.Term

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
mark :: Functor a => (Range -> History) -> Term a (Record (Range ': fields)) -> Term a (Record (History ': fields))
mark f = fmap go where go (r :. a) = f r :. a


data RPState = RPState
  { rpCursor  :: Int
  , rpHistory :: History
  , rpSource  :: Source
  }

data Reprinter a where
  Pure :: a -> Reprinter a
  Bind :: Reprinter a -> (a -> Reprinter b) -> Reprinter b

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

data Token
  = Chunk Source
    deriving (Show, Eq)

initial :: History -> Source -> RPState
initial = RPState 0

descend :: HasField fields History => SubtermAlgebra constr (Term a (Record fields)) (Reprinter ())
descend t = history >>= \case
  Pristine _   -> pure ()
  Modified _   -> pure ()
  Refactored _ -> pure ()
  Generated    -> pure ()

compile :: Reprinter a -> PlanT k Token (Eff '[State RPState]) a
compile r = case r of
  Get      -> lift get
  Put a    -> lift (put a)
  Pure v   -> pure v
  Bind p f -> compile p >>= compile . f
  Finish   -> compile (dropSource <$> cursor <*> source) >>= yield . Chunk

reprint :: (Functor a, HasField fields History) => Source -> Term a (Record fields) -> [Token]
reprint s t =
  run
  . fmap snd
  . runState (initial (getField (termAnnotation t)) s)
  . runT
  . repeatedly
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
--   whenPositioned = locally (withAnn (termFAnnotation )) (termFOut t)

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
