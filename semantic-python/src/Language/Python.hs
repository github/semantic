{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Semantic functionality for Python programs.
module Language.Python
( Term(..)
, TreeSitter.Python.tree_sitter_python
) where

-- import           Control.Carrier.Reader
-- import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import           Data.ScopeGraph (ToScopeGraph (..))
import qualified Data.ScopeGraph as ScopeGraph
import           GHC.Generics
import qualified Language.Python.Tags as PyTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Python (tree_sitter_python)
import qualified TreeSitter.Python.AST as Py
import qualified TreeSitter.Unmarshal as TS

todo :: Show a => a -> b
todo s = error ("TODO: " <> show s)

newtype Term a = Term { getTerm :: Py.Module a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . PyTags.tags . getTerm

instance ScopeGraph.ToScopeGraph Term where
  scopeGraph = ScopeGraph.scopeGraph . getTerm

instance ToScopeGraph Py.Block where
  scopeGraph (Py.Block _ children) = fold <$> traverse scopeGraph children

deriving instance ToScopeGraph Py.CompoundStatement

instance ToScopeGraph Py.DecoratedDefinition where scopeGraph = todo

instance ToScopeGraph Py.Module where
  scopeGraph Py.Module { Py.extraChildren = stmts } = fold <$> traverse scopeGraph stmts

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

deriving instance ToScopeGraph Py.Expression

instance ToScopeGraph Py.ElseClause where
  scopeGraph (Py.ElseClause _ elc) = scopeGraph elc

instance ToScopeGraph Py.ExceptClause where
  scopeGraph (Py.ExceptClause _ as) = fold <$> traverse scopeGraph as

instance ToScopeGraph Py.ExpressionList where
  scopeGraph (Py.ExpressionList _ as) = fold <$> traverse scopeGraph as

instance ToScopeGraph Py.FinallyClause where
  scopeGraph (Py.FinallyClause _ block) = scopeGraph block

instance ToScopeGraph Py.ForStatement where scopeGraph = todo

instance ToScopeGraph Py.FunctionDefinition where scopeGraph = todo

instance ToScopeGraph Py.IfStatement where
  scopeGraph (Py.IfStatement _ alternative body condition) = do
    con <- scopeGraph condition
    bod <- scopeGraph body
    alt <- traverse scopeGraph alternative
    pure (fold (con : bod : alt))

instance ToScopeGraph Py.Lambda where scopeGraph = todo

instance ToScopeGraph Py.NamedExpression where scopeGraph = todo

instance ToScopeGraph Py.ReturnStatement where
  scopeGraph (Py.ReturnStatement _ mVal) = maybe (pure mempty) scopeGraph mVal

instance ToScopeGraph Py.NotOperator where
  scopeGraph (Py.NotOperator _ arg) = scopeGraph arg

instance ToScopeGraph Py.PrintStatement where
  scopeGraph (Py.PrintStatement _ args _chevron) = fold <$> traverse scopeGraph args

deriving instance ToScopeGraph Py.PrimaryExpression

deriving instance ToScopeGraph Py.SimpleStatement

instance ToScopeGraph Py.RaiseStatement where
  scopeGraph = todo

instance ToScopeGraph Py.TryStatement where
  scopeGraph (Py.TryStatement _ body elseClauses) = do
    bod <- scopeGraph body
    els <- traverse scopeGraph elseClauses
    pure (fold (NonEmpty.cons bod els))

instance ToScopeGraph Py.UnaryOperator where
  scopeGraph (Py.UnaryOperator _ _ arg) = scopeGraph arg

instance ToScopeGraph Py.WhileStatement where
  scopeGraph Py.WhileStatement{ alternative, body, condition } = do
    con <- scopeGraph condition
    bod <- scopeGraph body
    alt <- maybe (pure mempty) scopeGraph alternative
    pure (con <> bod <> alt)

instance ToScopeGraph Py.WithStatement where
  scopeGraph = todo
