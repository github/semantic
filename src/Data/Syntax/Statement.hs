{-# LANGUAGE DeriveAnyClass, DerivingVia, ScopedTypeVariables, UndecidableInstances, ViewPatterns, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Statement where

import Prologue

import           Control.Abstract hiding (Break, Continue, Return, While)
import           Data.Abstract.Evaluatable as Abstract
import           Data.Aeson (ToJSON1 (..))
import           Data.JSON.Fields
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Map.Strict as Map
import qualified Data.Reprinting.Scope as Scope
import qualified Data.Reprinting.Token as Token
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import           Diffing.Algorithm
import           Reprinting.Tokenize (Tokenize (..), imperative, within', yield)

-- | Imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
--   TODO: Separate top-level statement nodes into non-lexical Statement and lexical StatementBlock nodes
newtype Statements a = Statements { statements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Statements

instance ToJSON1 Statements

instance Evaluatable Statements where
  eval eval _ (Statements xs) =
    maybe unit (runApp . foldMap1 (App . eval)) (nonEmpty xs)

instance Tokenize Statements where
  tokenize = imperative

newtype StatementBlock a = StatementBlock { statements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically StatementBlock

instance ToJSON1 StatementBlock

instance Evaluatable StatementBlock where
  eval eval _ (StatementBlock xs) =
    maybe unit (runApp . foldMap1 (App . eval)) (nonEmpty xs)

instance Tokenize StatementBlock where
  tokenize = imperative

-- | Conditional. This must have an else block, which can be filled with some default value when omitted in the source, e.g. 'pure ()' for C-style if-without-else or 'pure Nothing' for Ruby-style, in both cases assuming some appropriate Applicative context into which the If will be lifted.
data If a = If { ifCondition :: !a, ifThenBody :: !a, ifElseBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically If

instance Evaluatable If where
  eval eval _ (If cond if' else') = do
    bool <- eval cond
    ifthenelse bool (eval if') (eval else')

instance Tokenize If where
  tokenize If{..} = within' Scope.If $ do
    ifCondition
    yield (Token.Flow Token.Then)
    ifThenBody
    yield (Token.Flow Token.Else)
    ifElseBody

-- | Else statement. The else condition is any term, that upon successful completion, continues evaluation to the elseBody, e.g. `for ... else` in Python.
data Else a = Else { elseCondition :: !a, elseBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Else

-- TODO: Implement Eval instance for Else
instance Evaluatable Else

instance Tokenize Else where
  tokenize Else{..} = within' Scope.If (yield (Token.Flow Token.Else) *> elseCondition *> yield Token.Sep *> elseBody)

-- TODO: Alternative definition would flatten if/else if/else chains: data If a = If ![(a, a)] !(Maybe a)

-- | Goto statement (e.g. `goto a` in Go).
newtype Goto a = Goto { gotoLocation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Goto

-- TODO: Implement Eval instance for Goto
instance Evaluatable Goto

-- | A pattern-matching or computed jump control-flow statement, like 'switch' in C or JavaScript, or 'case' in Ruby or Haskell.
data Match a = Match { matchSubject :: !a, matchPatterns :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Match

-- TODO: Implement Eval instance for Match
instance Evaluatable Match

instance Tokenize Match where
  tokenize Match{..} = do
    yield (Token.Flow Token.Switch)
    matchSubject
    yield (Token.Flow Token.In) -- This may need further refinement
    matchPatterns

-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
data Pattern a = Pattern { value :: !a, patternBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Pattern

-- TODO: Implement Eval instance for Pattern
instance Evaluatable Pattern

instance Tokenize Pattern where
  tokenize Pattern{..} = within' Scope.Case (value *> patternBody)

-- | A let statement or local binding, like 'a as b' or 'let a = b'.
data Let a  = Let { letVariable :: !a, letValue :: !a, letBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Let

instance Evaluatable Let where
  eval eval _ Let{..} = do
    name <- maybeM (throwNoNameError letVariable) (declaredName letVariable)
    letSpan <- ask @Span
    valueName <- maybeM (throwNoNameError letValue) (declaredName letValue)
    assocScope <- associatedScope (Declaration valueName)

    _ <- withLexicalScopeAndFrame $ do
      declare (Declaration name) Default Public letSpan ScopeGraph.LetKind assocScope
      letVal <- eval letValue
      slot <- lookupSlot (Declaration name)
      assign slot letVal
      eval letBody
    unit


-- Assignment

-- | Assignment to a variable or other lvalue.
data Assignment a = Assignment { assignmentContext :: ![a], assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Assignment

instance Declarations1 Assignment where
  liftDeclaredName declaredName Assignment{..} = declaredName assignmentTarget

instance Evaluatable Assignment where
  eval eval ref Assignment{..} = do
    lhs <- ref assignmentTarget
    rhs <- eval assignmentValue

    case declaredName assignmentValue of
      Just rhsName -> do
        assocScope <- associatedScope (Declaration rhsName)
        case assocScope of
          Just assocScope' -> do
            objectScope <- newScope (Map.singleton Import [ assocScope' ])
            putSlotDeclarationScope lhs (Just objectScope) -- TODO: not sure if this is right
          Nothing ->
            pure ()
      Nothing ->
        pure ()
    assign lhs rhs
    pure rhs

instance Tokenize Assignment where
  -- Should we be using 'assignmentContext' in here?
  tokenize Assignment{..} = assignmentTarget *> yield Token.Assign <* assignmentValue

-- | Post increment operator (e.g. 1++ in Go, or i++ in C).
newtype PostIncrement a = PostIncrement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PostIncrement

-- TODO: Implement Eval instance for PostIncrement
instance Evaluatable PostIncrement


-- | Post decrement operator (e.g. 1-- in Go, or i-- in C).
newtype PostDecrement a = PostDecrement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PostDecrement

-- TODO: Implement Eval instance for PostDecrement
instance Evaluatable PostDecrement

-- | Pre increment operator (e.g. ++1 in C or Java).
newtype PreIncrement a = PreIncrement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PreIncrement

-- TODO: Implement Eval instance for PreIncrement
instance Evaluatable PreIncrement


-- | Pre decrement operator (e.g. --1 in C or Java).
newtype PreDecrement a = PreDecrement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PreDecrement

-- TODO: Implement Eval instance for PreDecrement
instance Evaluatable PreDecrement


-- Returns

newtype Return a = Return { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Return

instance Evaluatable Return where
  eval eval _ (Return x) = eval x >>= earlyReturn

instance Tokenize Return where
  tokenize (Return x) = within' Scope.Return x

newtype Yield a = Yield { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Yield

-- TODO: Implement Eval instance for Yield
instance Evaluatable Yield

instance Tokenize Yield where
  tokenize (Yield y) = yield (Token.Flow Token.Yield) *> y


newtype Break a = Break { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Break

instance Evaluatable Break where
  eval eval _ (Break x) = eval x >>= throwBreak

instance Tokenize Break where
  tokenize (Break b) = yield (Token.Flow Token.Break) *> b

newtype Continue a = Continue { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Continue

instance Evaluatable Continue where
  eval eval _ (Continue x) = eval x >>= throwContinue

instance Tokenize Continue where
  tokenize (Continue c) = yield (Token.Flow Token.Continue) *> c

newtype Retry a = Retry { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Retry

-- TODO: Implement Eval instance for Retry
instance Evaluatable Retry

instance Tokenize Retry where
  tokenize (Retry r) = yield (Token.Flow Token.Retry) *> r

newtype NoOp a = NoOp { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NoOp

instance Evaluatable NoOp where
  eval _ _ _ = unit

-- Loops

data For a = For { forBefore :: !a, forCondition :: !a, forStep :: !a, forBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically For

instance Evaluatable For where
  eval eval _ (fmap eval -> For before cond step body) = forLoop before cond step body

data ForEach a = ForEach { forEachBinding :: !a, forEachSubject :: !a, forEachBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ForEach

-- TODO: Implement Eval instance for ForEach
instance Evaluatable ForEach

instance Tokenize ForEach where
  tokenize ForEach{..} = within' Scope.Loop $ do
    yield (Token.Flow Token.Foreach)
    forEachBinding
    yield (Token.Flow Token.In)
    forEachSubject
    forEachBody

data While a = While { whileCondition :: !a, whileBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically While

instance Evaluatable While where
  eval eval _ While{..} = while (eval whileCondition) (eval whileBody)

instance Tokenize While where
  tokenize While{..} = within' Scope.Loop $ do
    yield (Token.Flow Token.While)
    whileCondition
    whileBody

data DoWhile a = DoWhile { doWhileCondition :: !a, doWhileBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically DoWhile

instance Evaluatable DoWhile where
  eval eval _ DoWhile{..} = doWhile (eval doWhileBody) (eval doWhileCondition)

-- Exception handling

newtype Throw a = Throw { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Throw

-- TODO: Implement Eval instance for Throw
instance Evaluatable Throw


data Try a = Try { tryBody :: !a, tryCatch :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Try

-- TODO: Implement Eval instance for Try
instance Evaluatable Try

instance Tokenize Try where
  tokenize Try{..} = do
    yield (Token.Flow Token.Try)
    tryBody
    yield (Token.Flow Token.Rescue)
    sequenceA_ tryCatch

data Catch a = Catch { catchException :: !a, catchBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Catch

-- TODO: Implement Eval instance for Catch
instance Evaluatable Catch

instance Tokenize Catch where
  tokenize Data.Syntax.Statement.Catch{..} = within' Scope.Catch $ catchException *> catchBody

newtype Finally a = Finally { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Finally

-- TODO: Implement Eval instance for Finally
instance Evaluatable Finally

instance Tokenize Finally where
  tokenize (Finally f) = within' Scope.Finally f

-- Scoping

-- | ScopeEntry (e.g. `BEGIN {}` block in Ruby or Perl).
newtype ScopeEntry a = ScopeEntry { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ScopeEntry

-- TODO: Implement Eval instance for ScopeEntry
instance Evaluatable ScopeEntry

instance Tokenize ScopeEntry where
  tokenize (ScopeEntry t) = within' Scope.BeginBlock (sequenceA_ t)


-- | ScopeExit (e.g. `END {}` block in Ruby or Perl).
newtype ScopeExit a = ScopeExit { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ScopeExit

-- TODO: Implement Eval instance for ScopeExit
instance Evaluatable ScopeExit

instance Tokenize ScopeExit where
  tokenize (ScopeExit t) = within' Scope.EndBlock (sequenceA_ t)
