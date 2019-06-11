{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleContexts, FlexibleInstances, RecordWildCards, StandaloneDeriving, TypeOperators #-}
module Language.Python.Core
( compile
) where

import Control.Monad.Fail
import Data.Core as Core
import GHC.Generics
import Prelude hiding (fail)
import TreeSitter.Python.AST as Py

class Compile t where
  -- FIXME: we should really try not to fail
  compile :: MonadFail m => t -> m Core
  default compile :: (MonadFail m, Show t) => t -> m Core
  compile = defaultCompile

defaultCompile :: (MonadFail m, Show t) => t -> m Core
defaultCompile t = fail $ "compilation unimplemented for " <> show t

instance (Compile l, Compile r) => Compile (Either l r) where compile = compileSum

instance Compile Py.AssertStatement
instance Compile Py.Attribute
instance Compile Py.Await
instance Compile Py.BinaryOperator
instance Compile Py.Block
instance Compile Py.BooleanOperator
instance Compile Py.BreakStatement
instance Compile Py.Call
instance Compile Py.ClassDefinition
instance Compile Py.ComparisonOperator

instance Compile Py.CompoundStatement where compile = compileSum

instance Compile Py.ConcatenatedString
instance Compile Py.ConditionalExpression
instance Compile Py.ContinueStatement
instance Compile Py.DecoratedDefinition
instance Compile Py.DeleteStatement
instance Compile Py.Dictionary
instance Compile Py.DictionaryComprehension
instance Compile Py.Ellipsis
instance Compile Py.ExecStatement

instance Compile Py.Expression where compile = compileSum

instance Compile Py.ExpressionStatement

instance Compile Py.False where compile _ = pure (Bool Prelude.False)

instance Compile Py.Float
instance Compile Py.ForStatement
instance Compile Py.FunctionDefinition
instance Compile Py.FutureImportStatement
instance Compile Py.GeneratorExpression
instance Compile Py.GlobalStatement
instance Compile Py.Identifier

instance Compile Py.IfStatement where
  compile IfStatement{..} = If <$> compile condition <*> compile consequence <*> case alternative of
    Nothing -> pure Unit
    Just clauses -> foldr clause (pure Unit) clauses
    where clause (Left  ElifClause{..}) rest = If <$> compile condition <*> compile consequence <*> rest
          clause (Right ElseClause{..}) _    = compile body

instance Compile Py.ImportFromStatement
instance Compile Py.ImportStatement
instance Compile Py.Integer
instance Compile Py.KeywordIdentifier
instance Compile Py.Lambda
instance Compile Py.List
instance Compile Py.ListComprehension

instance Compile Py.Module where
  compile (Module Nothing) = pure Unit
  compile (Module (Just statements)) = block <$> traverse compile statements

instance Compile Py.NamedExpression
instance Compile Py.None
instance Compile Py.NonlocalStatement
instance Compile Py.NotOperator
instance Compile Py.ParenthesizedExpression
instance Compile Py.PassStatement

instance Compile Py.PrimaryExpression where compile = compileSum

instance Compile Py.PrintStatement
instance Compile Py.ReturnStatement
instance Compile Py.RaiseStatement
instance Compile Py.Set
instance Compile Py.SetComprehension

instance Compile Py.SimpleStatement where compile = compileSum

instance Compile Py.String
instance Compile Py.Subscript

instance Compile Py.True where compile _ = pure (Bool Prelude.True)

instance Compile Py.TryStatement
instance Compile Py.Tuple
instance Compile Py.UnaryOperator
instance Compile Py.WhileStatement
instance Compile Py.WithStatement


compileSum :: (Generic t, GCompileSum (Rep t), MonadFail m) => t -> m Core
compileSum = gcompileSum . from

class GCompileSum f where
  gcompileSum :: MonadFail m => f a -> m Core

instance GCompileSum f => GCompileSum (M1 D d f) where
  gcompileSum (M1 f) = gcompileSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileSum (L1 l) = gcompileSum l
  gcompileSum (R1 r) = gcompileSum r

instance Compile t => GCompileSum (M1 C c (M1 S s (K1 R t))) where
  gcompileSum (M1 (M1 (K1 t))) = compile t
