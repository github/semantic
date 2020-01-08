{-# OPTIONS_GHC -freduction-depth=0 #-}
-- | Semantic functionality for TSX programs.
module Language.TSX
( Term(..)
, TreeSitter.TSX.tree_sitter_tsx
) where


import qualified Language.TSX.Tags as TsxTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.TSX (tree_sitter_tsx)
import qualified TreeSitter.TSX.AST as TSX
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: TSX.Program a }

instance TS.SymbolMatching Term where
  showFailure _ _ = "failed for Term"

instance TS.Unmarshal Term where
  matchers = fmap (TS.hoist Term) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . TsxTags.tags . getTerm
