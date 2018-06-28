{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module Assigning.Assignment.Deterministic where

import Data.Error
import qualified Data.Set as Set
import Data.Range
import Data.Span
import Prologue

class (Alternative f, Ord s, Show s) => Assigning s f | f -> s where
  sym :: s -> f s
  -- TODO: leafNode
  -- TODO: branchNode
  -- TODO: toTerm

combine :: Ord s => Bool -> Set s -> Set s -> Set s
combine e s1 s2 = if e then s1 <> s2 else lowerBound

data AST grammar = AST
  { astSymbol   :: !grammar
  , astRange    :: {-# UNPACK #-} !Range
  , astSpan     :: {-# UNPACK #-} !Span
  , astChildren :: ![AST grammar]
  }
  deriving (Eq, Ord, Show)

data State s = State
  { stateOffset :: {-# UNPACK #-} !Offset
  , stateInput  :: ![s]
  }
  deriving (Eq, Ord, Show)

stateSpan :: Measured Offset s => State s -> Span
stateSpan (State   (Offset _ ls cs) [])    = Span (Pos ls cs) (Pos ls cs)
stateSpan (State d@(Offset _ ls cs) (s:_)) = Span (Pos ls cs) (Pos ls' cs')
  where Offset _ ls' cs' = d <> measure s

advanceState :: Measured Offset s => State s -> State s
advanceState state@(State _ []) = state
advanceState (State o (s : ss)) = State (o <> measure s) ss

data Offset = Offset
  { offsetBytes :: {-# UNPACK #-} !Int
  , offsetLines :: {-# UNPACK #-} !Int
  , offsetCols  :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

instance Lower Offset where
  lowerBound = Offset 0 0 0

instance Semigroup Offset where
  Offset b1 l1 c1 <> Offset b2 l2 c2 = Offset (b1 + b2) (l1 + l2) (c1 + c2)

type Table s a = [(s, a)]

data Assignment s a = Assignment
  { assignEmpty :: Maybe a
  , firstSet    :: Set s
  , match       :: State s -> Set s -> Either (Error s) (State s, a)
  }
  deriving (Functor)

instance Ord s => Applicative (Assignment s) where
  pure a = Assignment (Just a) lowerBound (\ inp _ -> Right (inp, a))
  Assignment n1 f1 p1 <*> ~(Assignment n2 f2 p2) = Assignment (n1 <*> n2) (combine (isJust n1) f1 f2) (p1 `pseq` p2)
    where p1 `pseq` p2 = \ inp follow -> do
            (inp1, v1) <- p1 inp (combine (isJust n2) f2 follow)
            (inp2, v2) <- p2 inp1 follow
            let res = v1 v2
            res `seq` pure (inp2, res)

instance (Measured Offset s, Ord s) => Alternative (Assignment s) where
  empty = Assignment Nothing lowerBound (\ s _ -> Left (Error (stateSpan s) [] (listToMaybe (stateInput s))))
  Assignment n1 f1 p1 <|> Assignment n2 f2 p2 = Assignment (n1 <|> n2) (f1 <> f2) (p1 `palt` p2)
    where p1 `palt` p2 = p
            where p state@(State _ []) follow =
                    if      isJust n1 then p1 state follow
                    else if isJust n2 then p2 state follow
                    else Left (Error (stateSpan state) (toList (f1 <> f2)) Nothing)
                  p state@(State _ (s:_)) follow =
                    if      s `Set.member` f1 then p1 (advanceState state) follow
                    else if s `Set.member` f2 then p2 (advanceState state) follow
                    else if isJust n1 && s `Set.member` follow then p1 (advanceState state) follow
                    else if isJust n2 && s `Set.member` follow then p2 (advanceState state) follow
                    else Left (Error (stateSpan state) (toList (combine (isJust n1) f1 follow <> combine (isJust n2) f2 follow)) (Just s))

instance (Measured Offset s, Ord s, Show s) => Assigning s (Assignment s) where
  sym s = Assignment Nothing (Set.singleton s) (\ state _ -> case stateInput state of
    []  -> Left (Error (stateSpan state) [s] Nothing)
    _:_ -> Right (advanceState state, s))

invokeDet :: Assignment s a -> State s -> Either (Error s) a
invokeDet (Assignment _ _ p) inp = snd <$> p inp lowerBound


class Measured v a | a -> v where
  measure :: a -> v
