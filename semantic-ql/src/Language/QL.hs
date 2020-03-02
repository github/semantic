-- | Semantic functionality for QL programs.
module Language.QL
( Term(..)
, TreeSitter.QL.tree_sitter_ql
) where

import qualified AST.Unmarshal as TS
import           Data.Proxy
import qualified Language.QL.AST as QL
import qualified Language.QL.Tags as QLTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.QL (tree_sitter_ql)

newtype Term a = Term { getTerm :: QL.Ql a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy QL.Ql)
  showFailure _ = TS.showFailure (Proxy :: Proxy QL.Ql)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . QLTags.tags . getTerm
