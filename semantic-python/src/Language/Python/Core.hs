{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleContexts, FlexibleInstances, RecordWildCards, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Python.Core
( Compile
, compile
) where

import           Control.Monad.Fail
import           Data.Core as Core
import           Data.Name (Name)
import           Data.Name as Name
import           GHC.Generics
import           Prelude hiding (fail)
import qualified TreeSitter.Python.AST as Py

-- We can separate syntax nodes into several classes:
-- user references (Var)
-- user assignments (Let)
-- user definitions (Frame and :.)
-- simple values (Unit and :$)
-- preluded values, language builtins provided by Prelude
-- local control flow (for, while, continue, anything that stays within a scope)
-- external control flow (exception throwing, handling, await, yield, eval)
-- package relations (creation of a new module, import)
-- irresolvable

class MonadFail m => MonadPrelude m where
  lookupBuiltin :: Name -> m Core

instance MonadPrelude Maybe where
  lookupBuiltin = const Nothing

class Compile t where
  -- FIXME: we should really try not to fail
  compile :: MonadPrelude m => t -> m Core
  default compile :: (MonadPrelude m, Show t) => t -> m Core
  compile = defaultCompile

defaultCompile :: (MonadPrelude m, Show t) => t -> m Core
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

instance Compile Py.False where compile _ = lookupBuiltin (User "False")

instance Compile Py.Float
instance Compile Py.ForStatement

instance Compile Py.FunctionDefinition where
  compile Py.FunctionDefinition
    { name       = Py.Identifier name
    , parameters = Py.Parameters parameters
    , ..
    } = do
      parameters' <- params
      body' <- compile body
      pure (Let (User name) := lams parameters' body')
    where params = case parameters of
            Nothing -> pure []
            Just p  -> traverse param [p] -- FIXME: this is wrong in node-types.json, @p@ should already be a list
          param (Right (Right (Right (Left (Py.Identifier name))))) = pure (User name)
          param x = unimplemented x
          unimplemented x = fail $ "unimplemented: " <> show x

instance Compile Py.FutureImportStatement
instance Compile Py.GeneratorExpression
instance Compile Py.GlobalStatement

instance Compile Py.Identifier where
  compile (Py.Identifier text) = pure (Var (User text))

instance Compile Py.IfStatement where
  compile Py.IfStatement{..} = If <$> compile condition <*> compile consequence <*> case alternative of
    Nothing      -> pure Unit
    Just clauses -> foldr clause (pure Unit) clauses
    where clause (Left  Py.ElifClause{..}) rest = If <$> compile condition <*> compile consequence <*> rest
          clause (Right Py.ElseClause{..}) _    = compile body

instance Compile Py.ImportFromStatement
instance Compile Py.ImportStatement
instance Compile Py.Integer
--instance Compile Py.KeywordIdentifier
instance Compile Py.Lambda
instance Compile Py.List
instance Compile Py.ListComprehension

instance Compile Py.Module where
  compile (Py.Module Nothing)           = pure Unit
  compile (Py.Module (Just statements)) = block <$> traverse compile statements

instance Compile Py.NamedExpression
instance Compile Py.None
instance Compile Py.NonlocalStatement
instance Compile Py.NotOperator
instance Compile Py.ParenthesizedExpression

instance Compile Py.PassStatement where compile _ = pure Unit

instance Compile Py.PrimaryExpression where compile = compileSum

instance Compile Py.PrintStatement
instance Compile Py.ReturnStatement
instance Compile Py.RaiseStatement
instance Compile Py.Set
instance Compile Py.SetComprehension

instance Compile Py.SimpleStatement where compile = compileSum

instance Compile Py.String

instance Compile Py.Subscript

instance Compile Py.True where compile _ = lookupBuiltin (User "True")

instance Compile Py.TryStatement
instance Compile Py.Tuple
instance Compile Py.UnaryOperator
instance Compile Py.WhileStatement
instance Compile Py.WithStatement


compileSum :: (Generic t, GCompileSum (Rep t), MonadPrelude m) => t -> m Core
compileSum = gcompileSum . from

class GCompileSum f where
  gcompileSum :: MonadPrelude m => f a -> m Core

instance GCompileSum f => GCompileSum (M1 D d f) where
  gcompileSum (M1 f) = gcompileSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileSum (L1 l) = gcompileSum l
  gcompileSum (R1 r) = gcompileSum r

instance Compile t => GCompileSum (M1 C c (M1 S s (K1 R t))) where
  gcompileSum (M1 (M1 (K1 t))) = compile t
