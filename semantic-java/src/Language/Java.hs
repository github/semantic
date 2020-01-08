-- | Semantic functionality for Java programs.
module Language.Java
( Term(..)
, TreeSitter.Java.tree_sitter_java
) where

import qualified Language.Java.Tags as JavaTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Java (tree_sitter_java)
import qualified TreeSitter.Java.AST as Java
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: Java.Program a }

instance TS.SymbolMatching Term where
  showFailure _ _ = "failed for Term"

instance TS.Unmarshal Term where
  matchers = fmap (TS.hoist Term) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . JavaTags.tags . getTerm
