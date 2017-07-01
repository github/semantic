{-# LANGUAGE DeriveAnyClass, StandaloneDeriving #-}
module Data.Syntax.Statement where

import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics
import Prologue

-- | Conditional. This must have an else block, which can be filled with some default value when omitted in the source, e.g. 'pure ()' for C-style if-without-else or 'pure Nothing' for Ruby-style, in both cases assuming some appropriate Applicative context into which the If will be lifted.
data If a = If { ifCondition :: !a, ifThenBody :: !a, ifElseBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 If where liftEq = genericLiftEq
instance Show1 If where liftShowsPrec = genericLiftShowsPrec

-- | Else statement. The else condition is any term, that upon successful completion, continues evaluation to the elseBody, e.g. `for ... else` in Python.
data Else a = Else { elseCondition :: !a, elseBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Else where liftEq = genericLiftEq
instance Show1 Else where liftShowsPrec = genericLiftShowsPrec


-- TODO: Alternative definition would flatten if/else if/else chains: data If a = If ![(a, a)] !(Maybe a)

-- | A pattern-matching or computed jump control-flow statement, like 'switch' in C or JavaScript, or 'case' in Ruby or Haskell.
data Match with a = Match { matchSubject :: !a, matchPatterns :: ![with a] }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 with => Eq1 (Match with) where liftEq = genericLiftEq
instance Show1 with => Show1 (Match with) where liftShowsPrec = genericLiftShowsPrec

-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
newtype Pattern a = Pattern a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Pattern where liftEq = genericLiftEq
instance Show1 Pattern where liftShowsPrec = genericLiftShowsPrec


data Let a  = Let { letVariable :: !a, letValue :: !a, letBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Let where liftEq = genericLiftEq
instance Show1 Let where liftShowsPrec = genericLiftShowsPrec


-- Assignment

-- | Assignment to a variable or other lvalue.
data Assignment a = Assignment { assignmentTarget :: !a, assignmentValue :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Assignment where liftEq = genericLiftEq
instance Show1 Assignment where liftShowsPrec = genericLiftShowsPrec


-- Returns

newtype Return a = Return a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Return where liftEq = genericLiftEq
instance Show1 Return where liftShowsPrec = genericLiftShowsPrec

newtype Yield a = Yield a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Yield where liftEq = genericLiftEq
instance Show1 Yield where liftShowsPrec = genericLiftShowsPrec

newtype Break a = Break a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Break where liftEq = genericLiftEq
instance Show1 Break where liftShowsPrec = genericLiftShowsPrec

newtype Continue a = Continue a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Continue where liftEq = genericLiftEq
instance Show1 Continue where liftShowsPrec = genericLiftShowsPrec

newtype NoOp a = NoOp a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NoOp where liftEq = genericLiftEq
instance Show1 NoOp where liftShowsPrec = genericLiftShowsPrec


-- Loops

data For a = For { forBefore :: !a, forCondition :: !a, forStep :: !a, forBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 For where liftEq = genericLiftEq
instance Show1 For where liftShowsPrec = genericLiftShowsPrec

data ForEach a = ForEach { forEachBinding :: !a, forEachSubject :: !a, forEachBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ForEach where liftEq = genericLiftEq
instance Show1 ForEach where liftShowsPrec = genericLiftShowsPrec

data While a = While { whileCondition :: !a, whileBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 While where liftEq = genericLiftEq
instance Show1 While where liftShowsPrec = genericLiftShowsPrec

data DoWhile a = DoWhile { doWhileCondition :: !a, doWhileBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 DoWhile where liftEq = genericLiftEq
instance Show1 DoWhile where liftShowsPrec = genericLiftShowsPrec


-- Exception handling

newtype Throw a = Throw a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Throw where liftEq = genericLiftEq
instance Show1 Throw where liftShowsPrec = genericLiftShowsPrec

data Try a = Try !a ![a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Try where liftEq = genericLiftEq
instance Show1 Try where liftShowsPrec = genericLiftShowsPrec

data Catch a = Catch !(Maybe a) !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Catch where liftEq = genericLiftEq
instance Show1 Catch where liftShowsPrec = genericLiftShowsPrec

newtype Finally a = Finally a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Finally where liftEq = genericLiftEq
instance Show1 Finally where liftShowsPrec = genericLiftShowsPrec
