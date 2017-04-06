{-# LANGUAGE StandaloneDeriving #-}
module Data.Syntax.Statement where

import Data.Functor.Classes.Eq.Generic
import GHC.Generics
import Prologue

-- | Conditional. This must have an else block, which can be filled with some default value when omitted in the source, e.g. 'pure ()' for C-style if-without-else or 'pure Nothing' for Ruby-style, in both cases assuming some appropriate Applicative context into which the If will be lifted.
data If a = If { ifCondition :: !a, ifThenBody :: !a, ifElseBody :: !a }
  deriving (Eq, Generic1, Show)

instance Eq1 If where liftEq = genericLiftEq

-- TODO: Alternative definition would flatten if/else if/else chains: data If a = If ![(a, a)] !(Maybe a)

-- | A pattern-matching or computed jump control-flow statement, like 'switch' in C or JavaScript, or 'case' in Ruby or Haskell.
data Match with a = Switch { matchSubject :: !a, matchPatterns :: ![with a] }
  deriving (Eq, Generic1, Show)

instance Eq1 with => Eq1 (Match with) where liftEq = genericLiftEq

-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
newtype Pattern a = Pattern a
  deriving (Eq, Generic1, Show)

instance Eq1 Pattern where liftEq = genericLiftEq


-- Returns

newtype Return a = Return a
  deriving (Eq, Generic1, Show)

instance Eq1 Return where liftEq = genericLiftEq

newtype Yield a = Yield a
  deriving (Eq, Generic1, Show)

instance Eq1 Yield where liftEq = genericLiftEq


-- Loops

data For a = For { forBefore :: !a, forCondition :: !a, forStep :: !a, forBody :: !a }
  deriving (Eq, Generic1, Show)

instance Eq1 For where liftEq = genericLiftEq

data ForEach a = ForEach { forEachBinding :: !a, forEachSubject :: !a, forEachBody :: !a }
  deriving (Eq, Generic1, Show)

instance Eq1 ForEach where liftEq = genericLiftEq

data While a = While { whileCondition :: !a, whileBody :: !a }
  deriving (Eq, Generic1, Show)

instance Eq1 While where liftEq = genericLiftEq

data DoWhile a = DoWhile { doWhileCondition :: !a, doWhileBody :: !a }
  deriving (Eq, Generic1, Show)

instance Eq1 DoWhile where liftEq = genericLiftEq


-- Exception handling

newtype Throw a = Throw a
  deriving (Eq, Generic1, Show)

instance Eq1 Throw where liftEq = genericLiftEq

data Try with a = Try !a ![with a]
  deriving (Eq, Generic1, Show)
-- deriving instance (Eq a, Eq (with a)) => Eq (Try with a)
-- deriving instance (Show a, Show (with a)) => Show (Try with a)

instance Eq1 with => Eq1 (Try with) where liftEq = genericLiftEq

data Catch a = Catch !(Maybe a) !a
  deriving (Eq, Generic1, Show)

instance Eq1 Catch where liftEq = genericLiftEq

newtype Finally a = Finally a
  deriving (Eq, Generic1, Show)

instance Eq1 Finally where liftEq = genericLiftEq
