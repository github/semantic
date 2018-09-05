{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables, UndecidableInstances, ViewPatterns, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Statement where

import Data.Abstract.Evaluatable
import Data.Aeson (ToJSON1 (..))
import Data.JSON.Fields
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Diffing.Algorithm
import Prelude
import Prologue
import Proto3.Suite.Class
import Reprinting.Tokenize

-- | Imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
newtype Statements a = Statements { statements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Statements where liftEq = genericLiftEq
instance Ord1 Statements where liftCompare = genericLiftCompare
instance Show1 Statements where liftShowsPrec = genericLiftShowsPrec
instance ToJSON1 Statements

instance Evaluatable Statements where
  eval (Statements xs) = maybe (rvalBox unit) (runApp . foldMap1 (App . subtermRef)) (nonEmpty xs)

instance Tokenize Statements where
  tokenize = imperative

-- | Conditional. This must have an else block, which can be filled with some default value when omitted in the source, e.g. 'pure ()' for C-style if-without-else or 'pure Nothing' for Ruby-style, in both cases assuming some appropriate Applicative context into which the If will be lifted.
data If a = If { ifCondition :: !a, ifThenBody :: !a, ifElseBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 If where liftEq = genericLiftEq
instance Ord1 If where liftCompare = genericLiftCompare
instance Show1 If where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable If where
  eval (If cond if' else') = do
    bool <- subtermValue cond
    Rval <$> ifthenelse bool (subtermAddress if') (subtermAddress else')

instance Tokenize If where
  tokenize If{..} = within' TIf $ do
    ifCondition
    yield TThen
    ifThenBody
    yield TElse
    ifElseBody

-- | Else statement. The else condition is any term, that upon successful completion, continues evaluation to the elseBody, e.g. `for ... else` in Python.
data Else a = Else { elseCondition :: !a, elseBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Else where liftEq = genericLiftEq
instance Ord1 Else where liftCompare = genericLiftCompare
instance Show1 Else where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Else
instance Evaluatable Else

-- TODO: Alternative definition would flatten if/else if/else chains: data If a = If ![(a, a)] !(Maybe a)

-- | Goto statement (e.g. `goto a` in Go).
newtype Goto a = Goto { gotoLocation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Goto where liftEq = genericLiftEq
instance Ord1 Goto where liftCompare = genericLiftCompare
instance Show1 Goto where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Goto
instance Evaluatable Goto


-- | A pattern-matching or computed jump control-flow statement, like 'switch' in C or JavaScript, or 'case' in Ruby or Haskell.
data Match a = Match { matchSubject :: !a, matchPatterns :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Match where liftEq = genericLiftEq
instance Ord1 Match where liftCompare = genericLiftCompare
instance Show1 Match where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Match
instance Evaluatable Match


-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
data Pattern a = Pattern { value :: !a, patternBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Pattern where liftEq = genericLiftEq
instance Ord1 Pattern where liftCompare = genericLiftCompare
instance Show1 Pattern where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Pattern
instance Evaluatable Pattern


-- | A let statement or local binding, like 'a as b' or 'let a = b'.
data Let a  = Let { letVariable :: !a, letValue :: !a, letBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Let where liftEq = genericLiftEq
instance Ord1 Let where liftCompare = genericLiftCompare
instance Show1 Let where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Let where
  eval Let{..} = do
    name <- maybeM (throwEvalError NoNameError) (declaredName (subterm letVariable))
    addr <- snd <$> letrec name (subtermValue letValue)
    Rval <$> locally (bind name addr *> subtermAddress letBody)


-- Assignment

-- | Assignment to a variable or other lvalue.
data Assignment a = Assignment { assignmentContext :: ![a], assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Assignment where liftEq = genericLiftEq
instance Ord1 Assignment where liftCompare = genericLiftCompare
instance Show1 Assignment where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Assignment where
  eval Assignment{..} = do
    lhs <- subtermRef assignmentTarget
    rhs <- subtermAddress assignmentValue

    case lhs of
      LvalLocal nam -> do
        bind nam rhs
      LvalMember _ _ ->
        -- we don't yet support mutable object properties:
        pure ()
      Rval _ ->
        -- the left hand side of the assignment expression is invalid:
        pure ()

    pure (Rval rhs)

-- | Post increment operator (e.g. 1++ in Go, or i++ in C).
newtype PostIncrement a = PostIncrement a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 PostIncrement where liftEq = genericLiftEq
instance Ord1 PostIncrement where liftCompare = genericLiftCompare
instance Show1 PostIncrement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PostIncrement
instance Evaluatable PostIncrement


-- | Post decrement operator (e.g. 1-- in Go, or i-- in C).
newtype PostDecrement a = PostDecrement a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 PostDecrement where liftEq = genericLiftEq
instance Ord1 PostDecrement where liftCompare = genericLiftCompare
instance Show1 PostDecrement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PostDecrement
instance Evaluatable PostDecrement

-- | Pre increment operator (e.g. ++1 in C or Java).
newtype PreIncrement a = PreIncrement a
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1)

instance Eq1 PreIncrement where liftEq = genericLiftEq
instance Ord1 PreIncrement where liftCompare = genericLiftCompare
instance Show1 PreIncrement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PreIncrement
instance Evaluatable PreIncrement


-- | Pre decrement operator (e.g. --1 in C or Java).
newtype PreDecrement a = PreDecrement a
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1)

instance Eq1 PreDecrement where liftEq = genericLiftEq
instance Ord1 PreDecrement where liftCompare = genericLiftCompare
instance Show1 PreDecrement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PreDecrement
instance Evaluatable PreDecrement


-- Returns

newtype Return a = Return { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Return where liftEq = genericLiftEq
instance Ord1 Return where liftCompare = genericLiftCompare
instance Show1 Return where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Return where
  eval (Return x) = Rval <$> (subtermAddress x >>= earlyReturn)

instance Tokenize Return where
  tokenize (Return x) = within' TReturn x

newtype Yield a = Yield { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Yield where liftEq = genericLiftEq
instance Ord1 Yield where liftCompare = genericLiftCompare
instance Show1 Yield where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Yield
instance Evaluatable Yield


newtype Break a = Break { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Break where liftEq = genericLiftEq
instance Ord1 Break where liftCompare = genericLiftCompare
instance Show1 Break where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Break where
  eval (Break x) = Rval <$> (subtermAddress x >>= throwBreak)

newtype Continue a = Continue { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Continue where liftEq = genericLiftEq
instance Ord1 Continue where liftCompare = genericLiftCompare
instance Show1 Continue where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Continue where
  eval (Continue x) = Rval <$> (subtermAddress x >>= throwContinue)

newtype Retry a = Retry { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Retry where liftEq = genericLiftEq
instance Ord1 Retry where liftCompare = genericLiftCompare
instance Show1 Retry where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Retry
instance Evaluatable Retry


newtype NoOp a = NoOp { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 NoOp where liftEq = genericLiftEq
instance Ord1 NoOp where liftCompare = genericLiftCompare
instance Show1 NoOp where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NoOp where
  eval _ = rvalBox unit

-- Loops

data For a = For { forBefore :: !a, forCondition :: !a, forStep :: !a, forBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 For where liftEq = genericLiftEq
instance Ord1 For where liftCompare = genericLiftCompare
instance Show1 For where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable For where
  eval (fmap subtermValue -> For before cond step body) = rvalBox =<< forLoop before cond step body


data ForEach a = ForEach { forEachBinding :: !a, forEachSubject :: !a, forEachBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 ForEach where liftEq = genericLiftEq
instance Ord1 ForEach where liftCompare = genericLiftCompare
instance Show1 ForEach where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ForEach
instance Evaluatable ForEach


data While a = While { whileCondition :: !a, whileBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 While where liftEq = genericLiftEq
instance Ord1 While where liftCompare = genericLiftCompare
instance Show1 While where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable While where
  eval While{..} = rvalBox =<< while (subtermValue whileCondition) (subtermValue whileBody)

data DoWhile a = DoWhile { doWhileCondition :: !a, doWhileBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 DoWhile where liftEq = genericLiftEq
instance Ord1 DoWhile where liftCompare = genericLiftCompare
instance Show1 DoWhile where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DoWhile where
  eval DoWhile{..} = rvalBox =<< doWhile (subtermValue doWhileBody) (subtermValue doWhileCondition)

-- Exception handling

newtype Throw a = Throw { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Throw where liftEq = genericLiftEq
instance Ord1 Throw where liftCompare = genericLiftCompare
instance Show1 Throw where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Throw
instance Evaluatable Throw


data Try a = Try { tryBody :: !a, tryCatch :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Try where liftEq = genericLiftEq
instance Ord1 Try where liftCompare = genericLiftCompare
instance Show1 Try where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Try
instance Evaluatable Try


data Catch a = Catch { catchException :: !a, catchBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Catch where liftEq = genericLiftEq
instance Ord1 Catch where liftCompare = genericLiftCompare
instance Show1 Catch where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Catch
instance Evaluatable Catch


newtype Finally a = Finally { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Finally where liftEq = genericLiftEq
instance Ord1 Finally where liftCompare = genericLiftCompare
instance Show1 Finally where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Finally
instance Evaluatable Finally


-- Scoping

-- | ScopeEntry (e.g. `BEGIN {}` block in Ruby or Perl).
newtype ScopeEntry a = ScopeEntry { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 ScopeEntry where liftEq = genericLiftEq
instance Ord1 ScopeEntry where liftCompare = genericLiftCompare
instance Show1 ScopeEntry where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ScopeEntry
instance Evaluatable ScopeEntry


-- | ScopeExit (e.g. `END {}` block in Ruby or Perl).
newtype ScopeExit a = ScopeExit { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 ScopeExit where liftEq = genericLiftEq
instance Ord1 ScopeExit where liftCompare = genericLiftCompare
instance Show1 ScopeExit where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ScopeExit
instance Evaluatable ScopeExit
