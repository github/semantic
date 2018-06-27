{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module Assigning.Assignment.Deterministic where

import Data.Error
import qualified Data.Set as Set
import Data.Span
import Prologue

class (Alternative f, Ord s, Show s) => Assigning s f | f -> s where
  sym :: s -> f s

combine :: Ord s => Bool -> Set s -> Set s -> Set s
combine e s1 s2 = if e then s1 <> s2 else lowerBound

data State s = State
  { stateDelta :: {-# UNPACK #-} !Delta
  , stateInput :: ![s]
  }
  deriving (Eq, Ord, Show)

stateSpan :: Measured Delta s => State s -> Span
stateSpan (State   (Delta _ ls cs) [])    = Span (Pos ls cs) (Pos ls cs)
stateSpan (State d@(Delta _ ls cs) (s:_)) = Span (Pos ls cs) (Pos ls' cs')
  where Delta _ ls' cs' = d <> measure s

advanceState :: Measured Delta s => State s -> State s
advanceState state@(State _ []) = state
advanceState (State o (s : ss)) = State (o <> measure s) ss

data Delta = Delta
  { deltaBytes :: {-# UNPACK #-} !Int
  , deltaLines :: {-# UNPACK #-} !Int
  , deltaCols  :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

instance Lower Delta where
  lowerBound = Delta 0 0 0

instance Semigroup Delta where
  Delta b1 l1 c1 <> Delta b2 l2 c2 = Delta (b1 + b2) (l1 + l2) (c1 + c2)

type Table s a = [(s, a)]

data DetPar s a = DetPar
  { isNullable :: Bool
  , firstSet   :: Set s
  , match      :: State s -> Set s -> Either (Error s) (State s, a)
  }
  deriving (Functor)

instance Ord s => Applicative (DetPar s) where
  pure a = DetPar True lowerBound (\ inp _ -> Right (inp, a))
  DetPar n1 f1 p1 <*> ~(DetPar n2 f2 p2) = DetPar (n1 && n2) (combine n1 f1 f2) (p1 `pseq` p2)
    where p1 `pseq` p2 = \ inp follow -> do
            (inp1, v1) <- p1 inp (combine n2 f2 follow)
            (inp2, v2) <- p2 inp1 follow
            let res = v1 v2
            res `seq` pure (inp2, res)

instance (Measured Delta s, Ord s) => Alternative (DetPar s) where
  empty = DetPar False lowerBound (\ s _ -> Left (Error (stateSpan s) [] (listToMaybe (stateInput s))))
  DetPar n1 f1 p1 <|> DetPar n2 f2 p2 = DetPar (n1 || n2) (f1 <> f2) (p1 `palt` p2)
    where p1 `palt` p2 = p
            where p state@(State _ []) follow =
                    if      n1 then p1 state follow
                    else if n2 then p2 state follow
                    else Left (Error (stateSpan state) (toList (f1 <> f2)) Nothing)
                  p state@(State _ (s:_)) follow =
                    if      s `Set.member` f1 then p1 (advanceState state) follow
                    else if s `Set.member` f2 then p2 (advanceState state) follow
                    else if n1 && s `Set.member` follow then p1 (advanceState state) follow
                    else if n2 && s `Set.member` follow then p2 (advanceState state) follow
                    else Left (Error (stateSpan state) (toList (combine n1 f1 follow <> combine n2 f2 follow)) (Just s))

instance (Measured Delta s, Ord s, Show s) => Assigning s (DetPar s) where
  sym s = DetPar False (Set.singleton s) (\ state _ -> case stateInput state of
    []  -> Left (Error (stateSpan state) [s] Nothing)
    _:_ -> Right (advanceState state, s))

invokeDet :: DetPar s a -> State s -> Either (Error s) a
invokeDet (DetPar _ _ p) inp = snd <$> p inp lowerBound


class Measured v a | a -> v where
  measure :: a -> v
