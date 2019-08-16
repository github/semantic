{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleContexts, FlexibleInstances, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeOperators, ScopedTypeVariables, PartialTypeSignatures #-}
module Language.Python.Core
( compile
) where

import Prelude hiding (fail)

import           Control.Effect hiding ((:+:))
import           Control.Monad.Fail
import           Data.Core as Core
import           Data.Foldable
import           Data.List
import           Data.Name as Name
import           GHC.Generics
import qualified TreeSitter.Python.AST as Py
import qualified Data.Text.Encoding as Text

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

    -- data Assignment
    --   = Assignment {left :: ExpressionList,
    --                 right :: (Maybe (Either Assignment (Either AugmentedAssignment (Either ExpressionList Yield)))),
    --                 type' :: (Maybe Type)}

-- TODO what is this third field here
instance Compile Py.Assignment where
  compile (Py.Assignment (Py.ExpressionList [lhs]) (Just rhs) _) = do
    target <- compile lhs
    value  <- compile rhs
    pure (target .= value)
  compile (Py.Assignment (Py.ExpressionList hs) _ _) = fail ("too many lhs values: " <> show (length hs))
  compile (Py.Assignment _ Nothing _) = fail "cannot compile assignment with no rhs"

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
    kids <- traverse compile children
    pure $ do' (fmap (Nothing :<-) kids)

instance Compile Py.ExpressionList where
  compile (Py.ExpressionList exprs) = do
    kids <- traverse compile exprs
    pure $ do' (fmap (Nothing :<-) kids)


instance Compile Py.False
--instance Compile Py.False where compile _ = pure (Bool False)

instance Compile Py.Float
instance Compile Py.ForStatement

instance Compile Py.FunctionDefinition
-- instance Compile Py.FunctionDefinition where
--   compile Py.FunctionDefinition
--     { name       = Py.Identifier name
--     , parameters = Py.Parameters parameters
--     , ..
--     } = do
--       parameters' <- params
--       body' <- compile body
--       pure (Let (User name) := lams parameters' body')
--     where params = case parameters of
--             Nothing -> pure []
--             Just p  -> traverse param [p] -- FIXME: this is wrong in node-types.json, @p@ should already be a list
--           param (Right (Right (Right (Left (Py.Identifier name))))) = pure (User name)
--           param x = unimplemented x
--           unimplemented x = fail $ "unimplemented: " <> show x

instance Compile Py.FutureImportStatement
instance Compile Py.GeneratorExpression
instance Compile Py.GlobalStatement

instance Compile Py.Identifier where
  compile (Py.Identifier bytes) = pure (pure bytes)
-- instance Compile Py.Identifier where
--   compile (Py.Identifier text) = pure (Var (User text))

instance Compile Py.IfStatement
-- instance Compile Py.IfStatement where
--   compile Py.IfStatement{..} = If <$> compile condition <*> compile consequence <*> case alternative of
--     Nothing      -> pure Unit
--     Just clauses -> foldr clause (pure Unit) clauses
--     where clause (Left  Py.ElifClause{..}) rest = If <$> compile condition <*> compile consequence <*> rest
--           clause (Right Py.ElseClause{..}) _    = compile body

organizeBindings :: [t Name] -> [(Name, t Name)]
organizeBindings _ = []

instance Compile Py.ImportFromStatement
instance Compile Py.ImportStatement
instance Compile Py.Integer
instance Compile Py.Lambda
instance Compile Py.List
instance Compile Py.ListComprehension

instance Compile Py.Module where
  compile (Py.Module stmts) = do
    res <- traverse compile stmts
    let paired = organizeBindings res
    pure (record paired)

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

instance Compile Py.True
-- instance Compile Py.True where compile _ = pure (Bool True)

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
