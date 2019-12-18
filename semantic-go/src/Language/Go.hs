-- | Semantic functionality for Go programs.
module Language.Go
( Term(..)
, TreeSitter.Go.tree_sitter_go
) where


import qualified Language.Go.Tags as GoTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Go (tree_sitter_go)
import qualified TreeSitter.Go.AST as Go
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: Go.SourceFile a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . GoTags.tags . getTerm
