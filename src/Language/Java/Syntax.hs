{-# LANGUAGE DeriveAnyClass #-}
module Language.Java.Syntax where

import Data.Abstract.Evaluatable hiding (Label)
import Diffing.Algorithm
import Prologue

newtype ArrayType a = ArrayType ByteString
  deriving (Diffable, Eq, FreeVariables1, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Ord1 ArrayType where liftCompare = genericLiftCompare
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ArrayType
instance Evaluatable ArrayType

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval (Module iden xs) = do
    name <- either (throwEvalError . FreeVariablesError) pure (freeVariable $ subterm iden)
    letrec' name $ \addr ->
      eval xs <* makeNamespace name addr []
