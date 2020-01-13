{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

instance ToScopeGraph Py.Module where
  scopeGraph Py.Module { Py.extraChildren = stmts } = fold <$> traverse scopeGraph stmts

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

deriving instance ToScopeGraph Py.Expression

instance ToScopeGraph Py.ExpressionList where
  scopeGraph (Py.ExpressionList _ as) = fold <$> traverse scopeGraph as

deriving instance ToScopeGraph Py.CompoundStatement

instance ToScopeGraph Py.ReturnStatement where
  scopeGraph (Py.ReturnStatement _ mVal) = maybe (pure mempty) scopeGraph mVal

instance ToScopeGraph Py.NotOperator where
  scopeGraph (Py.NotOperator _ arg) = scopeGraph arg

deriving instance ToScopeGraph Py.SimpleStatement

instance ToScopeGraph Py.RaiseStatement where
  scopeGraph = todo
