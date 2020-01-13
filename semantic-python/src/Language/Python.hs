-- | Semantic functionality for Python programs.
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Python
( Term(..)
, TreeSitter.Python.tree_sitter_python
) where

-- import           Control.Carrier.Reader
-- import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.ScopeGraph (ToScopeGraph (..))
import qualified Data.ScopeGraph as ScopeGraph
import qualified Language.Python.Tags as PyTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Python (tree_sitter_python)
import qualified TreeSitter.Python.AST as Py
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: Py.Module a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . PyTags.tags . getTerm

instance ScopeGraph.ToScopeGraph Term where
  scopeGraph = ScopeGraph.scopeGraph . getTerm

instance ToScopeGraph Py.Module where
  scopeGraph Py.Module { Py.extraChildren = stmts } = fold <$> traverse scopeGraph stmts
