{-# LANGUAGE DefaultSignatures, OverloadedStrings, ScopedTypeVariables, NamedFieldPuns, NoRecordWildCards, TypeOperators #-}
module Language.Python.Core
( compile
) where

import Prelude hiding (fail)

import           Control.Effect hiding ((:+:))
import           Control.Monad.Fail
import           Data.Core as Core
import           Data.Foldable
import           Data.Name as Name
import           GHC.Generics
import qualified TreeSitter.Python.AST as Py

class Compile py where
  -- FIXME: we should really try not to fail
  compile :: (Member Core sig, Carrier sig t, Foldable t, MonadFail m) => py -> m (t Name)
  default compile :: (MonadFail m, Show py) => py -> m (t Name)
  compile = defaultCompile

defaultCompile :: (MonadFail m, Show py) => py -> m (t Name)
defaultCompile t = fail $ "compilation unimplemented for " <> show t

instance (Compile l, Compile r) => Compile (Either l r) where compile = compileSum

instance Compile Py.AssertStatement
instance Compile Py.Attribute

instance Compile Py.Assignment where
  compile (Py.Assignment (Py.ExpressionList [lhs]) (Just rhs) Nothing) = do
    target <- compile lhs
    value  <- compile rhs
    pure (target .= value)
  compile other = fail ("Unhandled assignment case: " <> show other)

instance Compile Py.AugmentedAssignment
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

instance Compile Py.ExpressionStatement where
  compile (Py.ExpressionStatement children) = do
    actions <- traverse compile children
    pure $ do' (fmap (Nothing :<-) actions)

instance Compile Py.ExpressionList where
  compile (Py.ExpressionList exprs) = do
    actions <- traverse compile exprs
    pure $ do' (fmap (Nothing :<-) actions)


instance Compile Py.False where compile _ = pure (bool False)

instance Compile Py.Float
instance Compile Py.ForStatement

instance Compile Py.FunctionDefinition where
  compile Py.FunctionDefinition
    { name       = Py.Identifier name
    , parameters = Py.Parameters parameters
    , body
    } = do
      parameters' <- params
      body' <- compile body
      pure (pure name .= lams parameters' body')
    where params = case parameters of
            Nothing -> pure []
            Just p  -> traverse param [p] -- FIXME: this is wrong in node-types.json, @p@ should already be a list
          param (Right (Right (Right (Left (Py.Identifier name))))) = pure (named' name)
          param x = unimplemented x
          unimplemented x = fail $ "unimplemented: " <> show x

instance Compile Py.FutureImportStatement
instance Compile Py.GeneratorExpression
instance Compile Py.GlobalStatement

instance Compile Py.Identifier where
  compile (Py.Identifier bytes) = pure (pure bytes)

instance Compile Py.IfStatement where
  compile Py.IfStatement{ condition, consequence, alternative } =
    if' <$> compile condition <*> compile consequence <*> foldr clause (pure unit) alternative
    where clause (Right Py.ElseClause{ body }) _ = compile body
          clause (Left  Py.ElifClause{ condition, consequence }) rest  =
            if' <$> compile condition <*> compile consequence <*> rest


instance Compile Py.ImportFromStatement
instance Compile Py.ImportStatement
instance Compile Py.Integer
instance Compile Py.Lambda
instance Compile Py.List
instance Compile Py.ListComprehension

instance Compile Py.Module where
  compile (Py.Module stmts) = do
    -- Buggy and ad-hoc: the toList call promotes too many variables
    -- to top-level scope.
    res <- traverse compile stmts
    let names = concatMap toList res
    pure . record $ zip names res

instance Compile Py.NamedExpression
instance Compile Py.None
instance Compile Py.NonlocalStatement
instance Compile Py.NotOperator
instance Compile Py.ParenthesizedExpression

instance Compile Py.PassStatement where
  compile (Py.PassStatement _) = pure Core.unit

instance Compile Py.PrimaryExpression where compile = compileSum

instance Compile Py.PrintStatement
instance Compile Py.ReturnStatement
instance Compile Py.RaiseStatement
instance Compile Py.Set
instance Compile Py.SetComprehension

instance Compile Py.SimpleStatement where compile = compileSum

instance Compile Py.String
instance Compile Py.Subscript

instance Compile Py.True where compile _ = pure (bool True)

instance Compile Py.TryStatement

instance Compile Py.Tuple where
  compile (Py.Tuple []) = pure Core.unit
  compile (Py.Tuple t)  = fail ("Unimplemented: non-empty tuple " <> show t)

instance Compile Py.UnaryOperator
instance Compile Py.WhileStatement
instance Compile Py.WithStatement
instance Compile Py.Yield

compileSum :: (Generic py, GCompileSum (Rep py), Member Core sig, Foldable t, Carrier sig t, MonadFail m) => py -> m (t Name)
compileSum = gcompileSum . from

class GCompileSum f where
  gcompileSum :: (Foldable t, Member Core sig, Carrier sig t, MonadFail m) => f a -> m (t Name)

instance GCompileSum f => GCompileSum (M1 D d f) where
  gcompileSum (M1 f) = gcompileSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileSum (L1 l) = gcompileSum l
  gcompileSum (R1 r) = gcompileSum r

instance Compile t => GCompileSum (M1 C c (M1 S s (K1 R t))) where
  gcompileSum (M1 (M1 (K1 t))) = compile t
