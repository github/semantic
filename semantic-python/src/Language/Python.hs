-- | Semantic functionality for Python programs.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Python
( Term(..)
, TreeSitter.Python.tree_sitter_python
) where

import qualified Algebra.Graph as Graph
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

instance ScopeGraph.ToScopeGraph Py.Module where
  scopeGraph Py.Module{} = pure $ ScopeGraph.ScopeGraph Graph.empty

--     parent <- ask
--     self <- ScopeGraph . G.vertex . Node Scope <$> liftIO newUnique
--     foldr (\item acc -> do {
--               x <- acc;
--               y <- scopeGraph src item;
--               pure (x --> y);
--           }) (pure (parent --> self)) stmts
