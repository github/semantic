{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures #-}
module Assigning.Assignment.Deterministic
( Assigning(..)
, TermAssigning(..)
, Assignment(..)
, TermAssignment(..)
) where

import Data.AST
import Data.Error
import qualified Data.Set as Set
import Data.Range
import Data.Record
import Data.Source as Source
import Data.Span
import Data.Term (Term, termIn, termAnnotation, termOut)
import Data.Text.Encoding (decodeUtf8')
import Prologue

class (Alternative f, Ord grammar, Show grammar) => Assigning grammar f | f -> grammar where
  leafNode   :: grammar -> f Text
  branchNode :: grammar -> f a -> f a

class Assigning grammar f => TermAssigning syntaxes grammar f | f -> grammar, f -> syntaxes where
  toTerm :: Element syntax syntaxes
         => f (syntax (Term (Sum syntaxes) (Record Location)))
         -> f         (Term (Sum syntaxes) (Record Location))

combine :: Ord s => Bool -> Set s -> Set s -> Set s
combine e s1 s2 = if e then s1 <> s2 else lowerBound

astSymbol :: AST [] grammar -> grammar
astSymbol = nodeSymbol . termAnnotation

astRange :: AST [] grammar -> Range
astRange = nodeByteRange . termAnnotation

astSpan :: AST [] grammar -> Span
astSpan = nodeSpan . termAnnotation

astChildren :: AST [] grammar -> [AST [] grammar]
astChildren = termOut

data State s = State
  { stateBytes :: {-# UNPACK #-} !Int
  , statePos   :: {-# UNPACK #-} !Pos
  , stateInput :: ![AST [] s]
  }
  deriving (Eq, Ord, Show)

stateRange :: State s -> Range
stateRange state@(State _ _ [])    = Range (stateBytes state) (stateBytes state)
stateRange       (State _ _ (s:_)) = astRange s

stateSpan :: State s -> Span
stateSpan state@(State _ _ [])    = Span (statePos state) (statePos state)
stateSpan       (State _ _ (s:_)) = astSpan s

stateLocation :: State s -> Record Location
stateLocation state = stateRange state :. stateSpan state :. Nil

advanceState :: State s -> State s
advanceState state
  | s:ss <- stateInput state = State (end (astRange s)) (spanEnd (astSpan s)) ss
  | otherwise                = state

type Table s a = [(s, a)]

data Assignment s a = Assignment
  { nullable :: Maybe (State s -> a)
  , firstSet :: Set s
  , match    :: Source -> State s -> Set s -> Either (Error (Either String s)) (State s, a)
  }
  deriving (Functor)

instance Ord s => Applicative (Assignment s) where
  pure a = Assignment (Just (const a)) lowerBound (\ _ state _ -> Right (state, a))
  Assignment n1 f1 p1 <*> ~(Assignment n2 f2 p2) = Assignment (liftA2 (<*>) n1 n2) (combine (isJust n1) f1 f2) (p1 `pseq` p2)
    where p1 `pseq` p2 = \ src inp follow -> do
            (inp1, v1) <- p1 src inp (combine (isJust n2) f2 follow)
            (inp2, v2) <- p2 src inp1 follow
            let res = v1 v2
            res `seq` pure (inp2, res)

instance Ord s => Alternative (Assignment s) where
  empty = Assignment Nothing lowerBound (\ _ s _ -> Left (Error (stateSpan s) [] (Right . astSymbol <$> listToMaybe (stateInput s))))
  Assignment n1 f1 p1 <|> Assignment n2 f2 p2 = Assignment (n1 <|> n2) (f1 <> f2) (p1 `palt` p2)
    where p1 `palt` p2 = p
            where p src state@(State _ _ []) follow =
                    if      isJust n1 then p1 src state follow
                    else if isJust n2 then p2 src state follow
                    else Left (Error (stateSpan state) (Right <$> toList (f1 <> f2)) Nothing)
                  p src state@(State _ _ (s:_)) follow =
                    if      astSymbol s `Set.member` f1 then p1 src (advanceState state) follow
                    else if astSymbol s `Set.member` f2 then p2 src (advanceState state) follow
                    else if isJust n1 && astSymbol s `Set.member` follow then p1 src (advanceState state) follow
                    else if isJust n2 && astSymbol s `Set.member` follow then p2 src (advanceState state) follow
                    else Left (Error (stateSpan state) (Right <$> toList (combine (isJust n1) f1 follow <> combine (isJust n2) f2 follow)) (Just (Right (astSymbol s))))

instance (Ord s, Show s) => Assigning s (Assignment s) where
  leafNode s = Assignment Nothing (Set.singleton s) (\ src state _ -> case stateInput state of
    []  -> Left (Error (stateSpan state) [Right s] Nothing)
    s:_ -> case decodeUtf8' (sourceBytes (Source.slice (astRange s) src)) of
      Left err   -> Left (Error (astSpan s) [Left "valid utf-8"] (Just (Left (show err))))
      Right text -> Right (advanceState state, text))

  branchNode s a = Assignment Nothing (Set.singleton s) (\ src state _ -> case stateInput state of
    []  -> Left (Error (stateSpan state) [Right s] Nothing)
    s:_ -> case runAssignment a src state { stateInput = astChildren s } of
      Left err -> Left err
      Right (state', a') -> case stateInput state' of
        []   -> Right (advanceState state, a')
        s':_ -> Left (Error (stateSpan state') [] (Just (Right (astSymbol s')))))

runAssignment :: Assignment s a -> Source -> State s -> Either (Error (Either String s)) (State s, a)
runAssignment (Assignment _ _ p) src inp = p src inp lowerBound


newtype TermAssignment (syntaxes :: [* -> *]) grammar a = TermAssignment { runTermAssignment :: Assignment grammar a }
  deriving (Alternative, Applicative, Functor, Assigning grammar)

instance (Ord grammar, Show grammar) => TermAssigning syntaxes grammar (TermAssignment syntaxes grammar) where
  toTerm (TermAssignment a) = TermAssignment (Assignment
    (case nullable a of
      Just f  -> Just (\ state -> termIn (stateLocation state) (inject (f state)))
      Nothing -> Nothing)
    (firstSet a)
    (\ src state follow -> case match a src state follow of
      Left err -> Left err
      Right (state', syntax) -> Right (state', termIn (stateLocation state) (inject syntax))))
