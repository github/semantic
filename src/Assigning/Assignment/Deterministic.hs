{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures #-}
module Assigning.Assignment.Deterministic
( Assigning(..)
, TermAssigning(..)
, parseError
, Assignment(..)
, runAssignment
, TermAssignment(..)
, State(..)
) where

import Data.AST
import Data.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Range
import Data.Record
import Data.Source as Source
import Data.Span
import qualified Data.Syntax as Syntax
import Data.Term (Term, termIn, termAnnotation, termOut)
import Data.Text.Encoding (decodeUtf8')
import Prologue

class (Alternative f, Ord symbol, Show symbol) => Assigning symbol f | f -> symbol where
  leafNode   :: symbol -> f Text
  branchNode :: symbol -> f a -> f a

class Assigning symbol f => TermAssigning syntaxes symbol f | f -> symbol, f -> syntaxes where
  toTerm :: Element syntax syntaxes
         => f (syntax (Term (Sum syntaxes) (Record Location)))
         -> f         (Term (Sum syntaxes) (Record Location))

parseError :: ( Bounded symbol
              , Element Syntax.Error syntaxes
              , HasCallStack
              , TermAssigning syntaxes symbol f
              )
           => f (Term (Sum syntaxes) (Record Location))
parseError = toTerm (leafNode maxBound $> Syntax.Error (Syntax.ErrorStack (getCallStack (freezeCallStack callStack))) [] (Just "ParseError") [])


data Assignment symbol a = Assignment
  { nullable :: Maybe (State symbol -> a)
  , firstSet :: Set symbol
  , choices  :: [(symbol, Cont symbol a)]
  }
  deriving (Functor)

type Cont symbol a = Source -> State symbol -> [Set symbol] -> Either (Error (Either String symbol)) (State symbol, a)

combine :: Ord symbol => Maybe a -> Set symbol -> Set symbol -> Set symbol
combine e s1 s2 = if isJust e then s1 <> s2 else lowerBound

choose :: Ord symbol
       => Maybe (State symbol -> a)
       -> Set symbol
       -> Map symbol (Cont symbol a)
       -> Cont symbol a
choose nullable firstSet table src state follow = case stateInput state of
  []  -> case nullable of
    Just f -> Right (state, f state)
    _      -> Left (Error (stateSpan state) (Right <$> toList firstSet) Nothing)
  s:_ -> case astSymbol s `Map.lookup` table of
    Just k -> k src state follow
    _      -> notFound (astSymbol s) state follow
  where notFound s state follow = case nullable of
          Just f | any (s `Set.member`) follow -> Right (state, f state)
          _                                    -> Left (Error (stateSpan state) (Right <$> toList firstSet) (Just (Right s)))

instance Ord symbol => Applicative (Assignment symbol) where
  pure a = Assignment (Just (const a)) lowerBound []
  Assignment n1 f1 t1 <*> ~(Assignment n2 f2 t2) = Assignment (liftA2 (<*>) n1 n2) (combine n1 f1 f2) (t1 `tseq` t2)
    where table2 = Map.fromList t2
          t1 `tseq` t2
            = map (fmap (\ p src state follow -> do
              (state', p') <- p src state (f2 : follow)
              (state'', q') <- choose n2 f2 table2 src state' follow
              let pq = p' q'
              pq `seq` pure (state'', pq))) t1
            <> case n1 of
              Just p -> map (fmap (\ q src state follow -> do
                let p' = p state
                (state', q') <- p' `seq` q src state follow
                let pq = p' q'
                pq `seq` pure (state', pq))) t2
              _ -> []

instance Ord symbol => Alternative (Assignment symbol) where
  empty = Assignment Nothing lowerBound []
  Assignment n1 f1 t1 <|> Assignment n2 f2 t2 = Assignment (n1 <|> n2) (f1 <> f2) (t1 <> t2)

instance (Ord symbol, Show symbol) => Assigning symbol (Assignment symbol) where
  leafNode s = Assignment Nothing (Set.singleton s)
    [ (s, \ src state _ -> case stateInput state of
      []  -> Left (Error (stateSpan state) [Right s] Nothing)
      s:_ -> case decodeUtf8' (sourceBytes (Source.slice (astRange s) src)) of
        Left err   -> Left (Error (astSpan s) [Left "valid utf-8"] (Just (Left (show err))))
        Right text -> Right (advanceState state, text))
    ]

  branchNode s a = Assignment Nothing (Set.singleton s)
    [ (s, \ src state _ -> case stateInput state of
      []  -> Left (Error (stateSpan state) [Right s] Nothing)
      s:_ -> first (const (advanceState state)) <$> runAssignment a src state { stateInput = astChildren s })
    ]

runAssignment :: Ord symbol => Assignment symbol a -> Source -> State symbol -> Either (Error (Either String symbol)) (State symbol, a)
runAssignment (Assignment nullable firstSet table) src input
  = case choose nullable firstSet (Map.fromList table) src input lowerBound of
    Left err -> Left err
    Right (state', a') -> case stateInput state' of
      []   -> Right (state', a')
      s':_ -> Left (Error (stateSpan state') [] (Just (Right (astSymbol s'))))


newtype TermAssignment (syntaxes :: [* -> *]) symbol a = TermAssignment { runTermAssignment :: Assignment symbol a }
  deriving (Alternative, Applicative, Functor, Assigning symbol)

instance (Ord symbol, Show symbol) => TermAssigning syntaxes symbol (TermAssignment syntaxes symbol) where
  toTerm (TermAssignment a) = TermAssignment (Assignment
    (case nullable a of
      Just f  -> Just (\ state -> termIn (stateLocation state) (inject (f state)))
      Nothing -> Nothing)
    (firstSet a)
    (map (fmap (\ match src state follow -> case match src state follow of
      Left err -> Left err
      Right (state', syntax) -> Right (state', termIn (stateLocation state) (inject syntax)))) (choices a)))


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


astSymbol :: AST [] symbol -> symbol
astSymbol = nodeSymbol . termAnnotation

astRange :: AST [] symbol -> Range
astRange = nodeByteRange . termAnnotation

astSpan :: AST [] symbol -> Span
astSpan = nodeSpan . termAnnotation

astChildren :: AST [] symbol -> [AST [] symbol]
astChildren = termOut
