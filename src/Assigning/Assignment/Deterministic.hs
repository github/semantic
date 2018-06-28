{-# LANGUAGE FunctionalDependencies #-}
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
  { stateBytes :: {-# UNPACK #-} !Int
  , statePos   :: {-# UNPACK #-} !Pos
  , stateInput :: ![AST s]
  }
  deriving (Eq, Ord, Show)

stateSpan :: State s -> Span
stateSpan (State _ (Pos l c) [])    = Span (Pos l c) (Pos l c)
stateSpan (State _ _         (s:_)) = astSpan s

advanceState :: State s -> State s
advanceState state
  | s:ss <- stateInput state = State (end (astRange s)) (spanEnd (astSpan s)) ss
  | otherwise                = state

type Table s a = [(s, a)]

data Assignment s a = Assignment
  { assignEmpty :: Maybe a
  , firstSet    :: Set s
  , match       :: State s -> Set s -> Either (Error (Either String s)) (State s, a)
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

instance Ord s => Alternative (Assignment s) where
  empty = Assignment Nothing lowerBound (\ s _ -> Left (Error (stateSpan s) [] (Right . astSymbol <$> listToMaybe (stateInput s))))
  Assignment n1 f1 p1 <|> Assignment n2 f2 p2 = Assignment (n1 <|> n2) (f1 <> f2) (p1 `palt` p2)
    where p1 `palt` p2 = p
            where p state@(State _ _ []) follow =
                    if      isJust n1 then p1 state follow
                    else if isJust n2 then p2 state follow
                    else Left (Error (stateSpan state) (Right <$> toList (f1 <> f2)) Nothing)
                  p state@(State _ _ (s:_)) follow =
                    if      astSymbol s `Set.member` f1 then p1 (advanceState state) follow
                    else if astSymbol s `Set.member` f2 then p2 (advanceState state) follow
                    else if isJust n1 && astSymbol s `Set.member` follow then p1 (advanceState state) follow
                    else if isJust n2 && astSymbol s `Set.member` follow then p2 (advanceState state) follow
                    else Left (Error (stateSpan state) (Right <$> toList (combine (isJust n1) f1 follow <> combine (isJust n2) f2 follow)) (Just (Right (astSymbol s))))

instance (Ord s, Show s) => Assigning s (Assignment s) where
  sym s = Assignment Nothing (Set.singleton s) (\ state _ -> case stateInput state of
    []  -> Left (Error (stateSpan state) [Right s] Nothing)
    _:_ -> Right (advanceState state, s))

invokeDet :: Assignment s a -> State s -> Either (Error (Either String s)) a
invokeDet (Assignment _ _ p) inp = snd <$> p inp lowerBound
