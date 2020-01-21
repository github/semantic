{-# OPTIONS_GHC -freduction-depth=0 #-}
-- | Semantic functionality for TypeScript programs.
module Language.TypeScript
( Term(..)
, TreeSitter.TypeScript.tree_sitter_typescript
) where

import           Data.Proxy
import qualified Language.TypeScript.Tags as TsTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.TypeScript (tree_sitter_typescript)
import qualified TreeSitter.TypeScript.AST as TypeScript
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: TypeScript.Program a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy TypeScript.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy TypeScript.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . TsTags.tags . getTerm
