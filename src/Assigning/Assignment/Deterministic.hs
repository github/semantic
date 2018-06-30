{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures #-}
module Assigning.Assignment.Deterministic
( Assigning(..)
, parseError
, Assignment(..)
, assign
, runAssignment
, State(..)
) where

import Data.AST
import Data.Error
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
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

  toTerm :: Element syntax syntaxes
         => f (syntax (Term (Sum syntaxes) (Record Location)))
         -> f         (Term (Sum syntaxes) (Record Location))

parseError :: ( Bounded symbol
              , Element Syntax.Error syntaxes
              , HasCallStack
              , Assigning symbol f
              )
           => f (Term (Sum syntaxes) (Record Location))
parseError = toTerm (leafNode maxBound $> Syntax.Error (Syntax.ErrorStack (getCallStack (freezeCallStack callStack))) [] (Just "ParseError") [])


data Assignment symbol a = Assignment
  { nullable :: Maybe (State symbol -> a)
  , firstSet :: IntSet
  , choices  :: [(symbol, Cont symbol a)]
  }
  deriving (Functor)

type Cont symbol a = Source -> State symbol -> [IntSet] -> Either (Error (Either String symbol)) (State symbol, a)

combine :: Maybe a -> IntSet -> IntSet -> IntSet
combine e s1 s2 = if isJust e then s1 <> s2 else lowerBound

choose :: Enum symbol
       => Maybe (State symbol -> a)
       -> IntSet
       -> IntMap (Cont symbol a)
       -> Cont symbol a
choose nullable firstSet table src state follow = case stateInput state of
  []  -> case nullable of
    Just f -> Right (state, f state)
    _      -> Left (Error (stateSpan state) (Right . toEnum <$> IntSet.toList firstSet) Nothing)
  s:_ -> case fromEnum (astSymbol s) `IntMap.lookup` table of
    Just k -> k src state follow
    _      -> notFound (astSymbol s) state follow
  where notFound s state follow = case nullable of
          Just f | any (fromEnum s `IntSet.member`) follow -> Right (state, f state)
          _                                                -> Left (Error (stateSpan state) (Right . toEnum <$> IntSet.toList firstSet) (Just (Right s)))

instance (Enum symbol, Ord symbol) => Applicative (Assignment symbol) where
  pure a = Assignment (Just (const a)) lowerBound []
  {-# INLINABLE pure #-}

  Assignment n1 f1 t1 <*> ~(Assignment n2 f2 t2) = Assignment (liftA2 (<*>) n1 n2) (combine n1 f1 f2) (t1 `tseq` t2)
    where table2 = IntMap.fromList (map (first fromEnum) t2)
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
  {-# INLINABLE (<*>) #-}

instance (Enum symbol, Ord symbol) => Alternative (Assignment symbol) where
  empty = Assignment Nothing lowerBound []
  {-# INLINABLE empty #-}

  Assignment n1 f1 t1 <|> Assignment n2 f2 t2 = Assignment (n1 <|> n2) (f1 <> f2) (t1 <> t2)
  {-# INLINABLE (<|>) #-}

instance (Enum symbol, Ord symbol, Show symbol) => Assigning symbol (Assignment symbol) where
  leafNode s = Assignment Nothing (IntSet.singleton (fromEnum s))
    [ (s, \ src state _ -> case stateInput state of
      []  -> Left (Error (stateSpan state) [Right s] Nothing)
      s:_ -> case decodeUtf8' (sourceBytes (Source.slice (astRange s) src)) of
        Left err   -> Left (Error (astSpan s) [Left "valid utf-8"] (Just (Left (show err))))
        Right text -> Right (advanceState state, text))
    ]

  branchNode s a = Assignment Nothing (IntSet.singleton (fromEnum s))
    [ (s, \ src state _ -> case stateInput state of
      []  -> Left (Error (stateSpan state) [Right s] Nothing)
      s:_ -> first (const (advanceState state)) <$> runAssignment a src state { stateInput = astChildren s })
    ]

  toTerm a = Assignment
    (case nullable a of
      Just f  -> Just (\ state -> termIn (stateLocation state) (inject (f state)))
      Nothing -> Nothing)
    (firstSet a)
    (map (fmap (\ match src state follow -> case match src state follow of
      Left err -> Left err
      Right (state', syntax) -> Right (state', termIn (stateLocation state) (inject syntax)))) (choices a))


assign :: (Enum symbol, Show symbol) => Assignment symbol a -> Source -> AST [] symbol -> Either (Error String) a
assign assignment src = bimap (fmap (either id show)) snd . runAssignment assignment src . State 0 lowerBound . pure

runAssignment :: Enum symbol => Assignment symbol a -> Source -> State symbol -> Either (Error (Either String symbol)) (State symbol, a)
runAssignment (Assignment nullable firstSet table) src input
  = case choose nullable firstSet (IntMap.fromList (map (first fromEnum) table)) src input lowerBound of
    Left err -> Left err
    Right (state', a') -> case stateInput state' of
      []   -> Right (state', a')
      s':_ -> Left (Error (stateSpan state') [] (Just (Right (astSymbol s'))))


data Nullable symbol a
  = Nullable (State symbol -> a)
  | NotNullable
  deriving (Functor)

instance Applicative (Nullable symbol) where
  pure = Nullable . const

  Nullable f <*> Nullable a = Nullable (\ state -> f state (a state))
  _          <*> _          = NotNullable

instance Alternative (Nullable symbol) where
  empty = NotNullable

  Nullable a  <|> _ = Nullable a
  NotNullable <|> b = b


data State symbol = State
  { stateBytes :: {-# UNPACK #-} !Int
  , statePos   :: {-# UNPACK #-} !Pos
  , stateInput :: ![AST [] symbol]
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
