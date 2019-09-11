{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DerivingStrategies, DerivingVia, DisambiguateRecordFields,
             FlexibleContexts, FlexibleInstances, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables,
             StandaloneDeriving, TypeOperators, UndecidableInstances, DeriveGeneric #-}
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

  compileCC :: (Member Core sig, Carrier sig t, Foldable t, MonadFail m) => py -> m (t Name) -> m (t Name)

  default compileCC :: ( Member Core sig
                       , Carrier sig t
                       , Foldable t
                       , MonadFail m
                       )
                    => py -> m (t Name) -> m (t Name)
  compileCC py cc = (>>>) <$> compile py <*> cc

-- | TODO: This is not right, it should be a reference to a Preluded
-- NoneType instance, but it will do for now.
none :: (Member Core sig, Carrier sig t) => t Name
none = unit

defaultCompile :: (MonadFail m, Show py) => py -> m (t Name)
defaultCompile t = fail $ "compilation unimplemented for " <> show t

newtype CompileSum py = CompileSum py

instance (Generic py, GCompileSum (Rep py)) => Compile (CompileSum py) where
  compile (CompileSum a) = compileSum a
  compileCC (CompileSum a) cc = compileCCSum a cc

deriving via CompileSum (Either l r) instance (Compile l, Compile r) => Compile (Either l r)

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

instance Compile Py.Block where
  compile t = compileCC t (pure none)

  -- The call to 'reverse' works around https://github.com/tree-sitter/haskell-tree-sitter/issues/195
  -- This will be obviated when we upgrade to tree-sitter-python 0.3
  compileCC (Py.Block body) cc = foldr compileCC cc (reverse body)

instance Compile Py.BooleanOperator
instance Compile Py.BreakStatement
instance Compile Py.Call
instance Compile Py.ClassDefinition
instance Compile Py.ComparisonOperator

deriving via CompileSum Py.CompoundStatement instance Compile Py.CompoundStatement

instance Compile Py.ConcatenatedString
instance Compile Py.ConditionalExpression
instance Compile Py.ContinueStatement
instance Compile Py.DecoratedDefinition
instance Compile Py.DeleteStatement
instance Compile Py.Dictionary
instance Compile Py.DictionaryComprehension
instance Compile Py.Ellipsis
instance Compile Py.ExecStatement

deriving via CompileSum Py.Expression instance Compile Py.Expression

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
          param x                                                   = unimplemented x
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

  compileCC Py.IfStatement{ condition, consequence, alternative} cc =
    if' <$> compile condition <*> compileCC consequence cc <*> foldr clause cc alternative
    where clause (Right Py.ElseClause{ body }) _ = compileCC body cc
          clause (Left  Py.ElifClause{ condition, consequence }) rest  =
            if' <$> compile condition <*> compileCC consequence cc <*> rest


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

deriving via CompileSum Py.PrimaryExpression instance Compile Py.PrimaryExpression

instance Compile Py.PrintStatement

instance Compile Py.ReturnStatement where
  compile (Py.ReturnStatement [])    = pure none
  compile (Py.ReturnStatement [val]) = compile val
  compile (Py.ReturnStatement vals)  = fail ("unimplemented: return statement returning " <> show (length vals) <> " values")

  compileCC r _ = compile r


instance Compile Py.RaiseStatement
instance Compile Py.Set
instance Compile Py.SetComprehension

deriving via CompileSum Py.SimpleStatement instance Compile Py.SimpleStatement

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

compileCCSum :: (Generic py, GCompileSum (Rep py), Member Core sig, Foldable t, Carrier sig t, MonadFail m) => py -> m (t Name) -> m (t Name)
compileCCSum = gcompileCCSum . from

class GCompileSum f where
  gcompileSum :: (Foldable t, Member Core sig, Carrier sig t, MonadFail m) => f a -> m (t Name)

  gcompileCCSum :: (Foldable t, Member Core sig, Carrier sig t, MonadFail m) => f a -> m (t Name) -> m (t Name)

instance GCompileSum f => GCompileSum (M1 D d f) where
  gcompileSum (M1 f) = gcompileSum f
  gcompileCCSum (M1 f) = gcompileCCSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileSum (L1 l) = gcompileSum l
  gcompileSum (R1 r) = gcompileSum r

  gcompileCCSum (L1 l) = gcompileCCSum l
  gcompileCCSum (R1 r) = gcompileCCSum r

instance Compile t => GCompileSum (M1 C c (M1 S s (K1 R t))) where
  gcompileSum (M1 (M1 (K1 t))) = compile t
  gcompileCCSum (M1 (M1 (K1 t))) = compileCC t
