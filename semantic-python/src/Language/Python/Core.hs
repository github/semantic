{-# LANGUAGE DefaultSignatures, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, OverloadedStrings, OverloadedLists, ScopedTypeVariables, NamedFieldPuns, TypeOperators #-}
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
import           TreeSitter.Span (Span)

class Compile py where
  -- FIXME: we should really try not to fail
  compile :: (Member Core sig, Carrier sig t, Foldable t, MonadFail m) => py -> m (t Name)
  default compile :: (MonadFail m, Show py) => py -> m (t Name)
  compile = defaultCompile

defaultCompile :: (MonadFail m, Show py) => py -> m (t Name)
defaultCompile t = fail $ "compilation unimplemented for " <> show t

instance (Compile l, Compile r) => Compile (Either l r) where compile = compileSum

instance Compile (Py.AssertStatement Span)
instance Compile (Py.Attribute Span)

instance Compile (Py.Assignment Span) where
  compile Py.Assignment { Py.left = Py.ExpressionList { Py.extraChildren = [lhs] }, Py.right = Just rhs } = do
    target <- compile lhs
    value  <- compile rhs
    pure (target .= value)
  compile other = fail ("Unhandled assignment case: " <> show other)

instance Compile (Py.AugmentedAssignment Span)
instance Compile (Py.Await Span)
instance Compile (Py.BinaryOperator Span)
instance Compile (Py.Block Span)
instance Compile (Py.BooleanOperator Span)
instance Compile (Py.BreakStatement Span)
instance Compile (Py.Call Span)
instance Compile (Py.ClassDefinition Span)
instance Compile (Py.ComparisonOperator Span)

instance Compile (Py.CompoundStatement Span) where compile = compileSum

instance Compile (Py.ConcatenatedString Span)
instance Compile (Py.ConditionalExpression Span)
instance Compile (Py.ContinueStatement Span)
instance Compile (Py.DecoratedDefinition Span)
instance Compile (Py.DeleteStatement Span)
instance Compile (Py.Dictionary Span)
instance Compile (Py.DictionaryComprehension Span)
instance Compile (Py.Ellipsis Span)
instance Compile (Py.ExecStatement Span)

instance Compile (Py.Expression Span) where compile = compileSum

instance Compile (Py.ExpressionStatement Span) where
  compile Py.ExpressionStatement { Py.extraChildren = children } = do
    actions <- traverse compile children
    pure $ do' (fmap (Nothing :<-) actions)

instance Compile (Py.ExpressionList Span) where
  compile Py.ExpressionList { Py.extraChildren = exprs } = do
    actions <- traverse compile exprs
    pure $ do' (fmap (Nothing :<-) actions)


instance Compile (Py.False Span) where compile _ = pure (bool False)

instance Compile (Py.Float Span)
instance Compile (Py.ForStatement Span)

instance Compile (Py.FunctionDefinition Span) where
  compile Py.FunctionDefinition
    { name       = Py.Identifier _ann1 name
    , parameters = Py.Parameters _ann2 parameters
    , body
    } = do
      parameters' <- traverse param parameters
      body' <- compile body
      pure (pure name .= lams parameters' body')
    where param (Py.IdentifierParameter (Py.Identifier _pann pname)) = pure (named' pname)
          param x = unimplemented x
          unimplemented x = fail $ "unimplemented: " <> show x

instance Compile (Py.FutureImportStatement Span)
instance Compile (Py.GeneratorExpression Span)
instance Compile (Py.GlobalStatement Span)

instance Compile (Py.Identifier Span) where
  compile Py.Identifier { bytes } = pure (pure bytes)

instance Compile (Py.IfStatement Span) where
  compile Py.IfStatement{ condition, consequence, alternative } =
    if' <$> compile condition <*> compile consequence <*> foldr clause (pure unit) alternative
    where clause (Right Py.ElseClause{ body }) _ = compile body
          clause (Left  Py.ElifClause{ condition, consequence }) rest  =
            if' <$> compile condition <*> compile consequence <*> rest


instance Compile (Py.ImportFromStatement Span)
instance Compile (Py.ImportStatement Span)
instance Compile (Py.Integer Span)
instance Compile (Py.Lambda Span)
instance Compile (Py.List Span)
instance Compile (Py.ListComprehension Span)

instance Compile (Py.Module Span) where
  compile Py.Module { Py.extraChildren = stmts } = do
    -- Buggy and ad-hoc: the toList call promotes too many variables
    -- to top-level scope.
    res <- traverse compile stmts
    let names = concatMap toList res
    pure . record $ zip names res

instance Compile (Py.NamedExpression Span)
instance Compile (Py.None Span)
instance Compile (Py.NonlocalStatement Span)
instance Compile (Py.NotOperator Span)
instance Compile (Py.ParenthesizedExpression Span)

instance Compile (Py.PassStatement Span) where
  compile Py.PassStatement {} = pure Core.unit

instance Compile (Py.PrimaryExpression Span) where compile = compileSum

instance Compile (Py.PrintStatement Span)
instance Compile (Py.ReturnStatement Span)
instance Compile (Py.RaiseStatement Span)
instance Compile (Py.Set Span)
instance Compile (Py.SetComprehension Span)

instance Compile (Py.SimpleStatement Span) where compile = compileSum

instance Compile (Py.String Span)
instance Compile (Py.Subscript Span)

instance Compile (Py.True Span) where compile _ = pure (bool True)

instance Compile (Py.TryStatement Span)

instance Compile (Py.Tuple Span) where
  compile Py.Tuple { Py.extraChildren = [] } = pure Core.unit
  compile t                                  = fail ("Unimplemented: non-empty tuple " <> show t)

instance Compile (Py.UnaryOperator Span)
instance Compile (Py.WhileStatement Span)
instance Compile (Py.WithStatement Span)
instance Compile (Py.Yield Span)

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
