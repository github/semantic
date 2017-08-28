{-# LANGUAGE DeriveAnyClass, StandaloneDeriving #-}
module Data.Syntax.Statement where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Pretty.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics

-- | Conditional. This must have an else block, which can be filled with some default value when omitted in the source, e.g. 'pure ()' for C-style if-without-else or 'pure Nothing' for Ruby-style, in both cases assuming some appropriate Applicative context into which the If will be lifted.
data If a = If { ifCondition :: !a, ifThenBody :: !a, ifElseBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 If where liftEq = genericLiftEq
instance Show1 If where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 If where liftPretty = genericLiftPretty

-- | Else statement. The else condition is any term, that upon successful completion, continues evaluation to the elseBody, e.g. `for ... else` in Python.
data Else a = Else { elseCondition :: !a, elseBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Else where liftEq = genericLiftEq
instance Show1 Else where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Else where liftPretty = genericLiftPretty

-- TODO: Alternative definition would flatten if/else if/else chains: data If a = If ![(a, a)] !(Maybe a)

-- | A pattern-matching or computed jump control-flow statement, like 'switch' in C or JavaScript, or 'case' in Ruby or Haskell.
data Match a = Match { matchSubject :: !a, matchPatterns :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Match where liftEq = genericLiftEq
instance Show1 Match where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Match where liftPretty = genericLiftPretty

-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
data Pattern a = Pattern { pattern :: !a, patternBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Pattern where liftEq = genericLiftEq
instance Show1 Pattern where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Pattern where liftPretty = genericLiftPretty

-- | A let statement or local binding, like 'a as b' or 'let a = b'.
data Let a  = Let { letVariable :: !a, letValue :: !a, letBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Let where liftEq = genericLiftEq
instance Show1 Let where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Let where liftPretty = genericLiftPretty


-- Assignment

-- | Assignment to a variable or other lvalue.
data Assignment a = Assignment { assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Assignment where liftEq = genericLiftEq
instance Show1 Assignment where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Assignment where liftPretty = genericLiftPretty


-- Returns

newtype Return a = Return a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Return where liftEq = genericLiftEq
instance Show1 Return where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Return where liftPretty = genericLiftPretty

newtype Yield a = Yield a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Yield where liftEq = genericLiftEq
instance Show1 Yield where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Yield where liftPretty = genericLiftPretty

newtype Break a = Break a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Break where liftEq = genericLiftEq
instance Show1 Break where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Break where liftPretty = genericLiftPretty

newtype Continue a = Continue a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Continue where liftEq = genericLiftEq
instance Show1 Continue where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Continue where liftPretty = genericLiftPretty

newtype Retry a = Retry a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Retry where liftEq = genericLiftEq
instance Show1 Retry where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Retry where liftPretty = genericLiftPretty

newtype NoOp a = NoOp a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NoOp where liftEq = genericLiftEq
instance Show1 NoOp where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 NoOp where liftPretty = genericLiftPretty


-- Loops

data For a = For { forBefore :: !a, forCondition :: !a, forStep :: !a, forBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 For where liftEq = genericLiftEq
instance Show1 For where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 For where liftPretty = genericLiftPretty

data ForEach a = ForEach { forEachBinding :: !a, forEachSubject :: !a, forEachBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ForEach where liftEq = genericLiftEq
instance Show1 ForEach where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 ForEach where liftPretty = genericLiftPretty

data While a = While { whileCondition :: !a, whileBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 While where liftEq = genericLiftEq
instance Show1 While where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 While where liftPretty = genericLiftPretty

data DoWhile a = DoWhile { doWhileCondition :: !a, doWhileBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 DoWhile where liftEq = genericLiftEq
instance Show1 DoWhile where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 DoWhile where liftPretty = genericLiftPretty


-- Exception handling

newtype Throw a = Throw a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Throw where liftEq = genericLiftEq
instance Show1 Throw where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Throw where liftPretty = genericLiftPretty

data Try a = Try { tryBody :: !a, tryCatch :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Try where liftEq = genericLiftEq
instance Show1 Try where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Try where liftPretty = genericLiftPretty

data Catch a = Catch { catchException :: !a, catchBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Catch where liftEq = genericLiftEq
instance Show1 Catch where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Catch where liftPretty = genericLiftPretty

newtype Finally a = Finally a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Finally where liftEq = genericLiftEq
instance Show1 Finally where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Finally where liftPretty = genericLiftPretty


-- | ScopeEntry (e.g. `BEGIN {}` block in Ruby or Perl).
newtype ScopeEntry a = ScopeEntry [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ScopeEntry where liftEq = genericLiftEq
instance Show1 ScopeEntry where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 ScopeEntry where liftPretty = genericLiftPretty


-- | ScopeExit (e.g. `END {}` block in Ruby or Perl).
newtype ScopeExit a = ScopeExit [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ScopeExit where liftEq = genericLiftEq
instance Show1 ScopeExit where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 ScopeExit where liftPretty = genericLiftPretty
