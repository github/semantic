{-# LANGUAGE StandaloneDeriving #-}
module Data.Syntax.Statement where

import Prologue

-- | Conditional. This must have an else block, which can be filled with some default value when omitted in the source, e.g. 'pure ()' for C-style if-without-else or 'pure Nothing' for Ruby-style, in both cases assuming some appropriate Applicative context into which the If will be lifted.
data If a = If { ifCondition :: !a, ifThenBody :: !a, ifElseBody :: !a }
  deriving (Eq, Show)

-- TODO: Alternative definition would flatten if/else if/else chains: data If a = If ![(a, a)] !(Maybe a)

-- | A pattern-matching or computed jump control-flow statement, like 'switch' in C or JavaScript, or 'case' in Ruby or Haskell.
data Match with a = Switch { matchSubject :: !a, matchPatterns :: ![with a] }
  deriving (Eq, Show)

-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
newtype Pattern a = Pattern a
  deriving (Eq, Show)


-- Returns

newtype Return a = Return a
  deriving (Eq, Show)

newtype Yield a = Yield a
  deriving (Eq, Show)


-- Loops

data For a = For { forBefore :: !a, forCondition :: !a, forStep :: !a, forBody :: !a }
  deriving (Eq, Show)

data ForEach a = ForEach { forEachBinding :: !a, forEachSubject :: !a, forEachBody :: !a }
  deriving (Eq, Show)

data While a = While { whileCondition :: !a, whileBody :: !a }
  deriving (Eq, Show)

data DoWhile a = DoWhile { doWhileCondition :: !a, doWhileBody :: !a }
  deriving (Eq, Show)


-- Exception handling

newtype Throw a = Throw a
  deriving (Eq, Show)

data Try with a = Try !a ![with a]
deriving instance (Eq a, Eq (with a)) => Eq (Try with a)
deriving instance (Show a, Show (with a)) => Show (Try with a)

data Catch a = Catch !(Maybe a) !a
  deriving (Eq, Show)

newtype Finally a = Finally a
  deriving (Eq, Show)
